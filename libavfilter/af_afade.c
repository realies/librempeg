/*
 * Copyright (c) 2013-2015 Paul B Mahol
 *
 * This file is part of Librempeg
 *
 * Librempeg is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * Librempeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with Librempeg; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

/**
 * @file
 * fade audio filter
 */

#include "config_components.h"

#include <inttypes.h>
#include "libavutil/avassert.h"
#include "libavutil/opt.h"
#include "audio.h"
#include "avfilter.h"
#include "filters.h"

typedef struct AudioFadeContext {
    const AVClass *class;
    int type;
    int curve, curve2;
    int64_t nb_samples;
    int64_t start_sample;
    int64_t duration;
    int64_t start_time;
    double silence;
    double unity;
    int overlap;
    int status[2];
    int passthrough;
    int64_t pts;

    /* Ring buffer for lazy crossfade (memory-efficient) */
    AVFrame *ring_buf;          /* Ring buffer holding last nb_samples from input 0 */
    int64_t ring_write_pos;     /* Write position in ring buffer (monotonically increasing) */
    int64_t ring_filled;        /* Number of valid samples in ring buffer */
    int64_t crossfade_pos;      /* Current position within crossfade (0 to nb_samples) */
    int crossfade_active;       /* Flag: currently in crossfade processing mode */

    void (*fade_samples)(uint8_t **dst, uint8_t * const *src,
                         int nb_samples, int channels, int direction,
                         int64_t start, int64_t range, int curve,
                         double silence, double unity);
    void (*scale_samples)(uint8_t **dst, uint8_t * const *src,
                          int nb_samples, int channels, double unity);
    void (*crossfade_samples)(uint8_t **dst, uint8_t * const *cf0,
                              uint8_t * const *cf1,
                              int nb_samples, int channels,
                              int curve0, int curve1,
                              int64_t offset, int64_t total);
} AudioFadeContext;

enum CurveType { NONE = -1, TRI, QSIN, ESIN, HSIN, LOG, IPAR, QUA, CUB, SQU, CBR, PAR, EXP, IQSIN, IHSIN, DESE, DESI, LOSI, SINC, ISINC, QUAT, QUATR, QSIN2, HSIN2, NB_CURVES };

#define OFFSET(x) offsetof(AudioFadeContext, x)
#define FLAGS AV_OPT_FLAG_AUDIO_PARAM|AV_OPT_FLAG_FILTERING_PARAM
#define TFLAGS AV_OPT_FLAG_AUDIO_PARAM|AV_OPT_FLAG_FILTERING_PARAM|AV_OPT_FLAG_RUNTIME_PARAM

    static const enum AVSampleFormat sample_fmts[] = {
        AV_SAMPLE_FMT_S16, AV_SAMPLE_FMT_S16P,
        AV_SAMPLE_FMT_S32, AV_SAMPLE_FMT_S32P,
        AV_SAMPLE_FMT_FLT, AV_SAMPLE_FMT_FLTP,
        AV_SAMPLE_FMT_DBL, AV_SAMPLE_FMT_DBLP,
        AV_SAMPLE_FMT_NONE
    };

#define DEPTH 16
#include "afade_template.c"

#undef DEPTH
#define DEPTH 31
#include "afade_template.c"

#undef DEPTH
#define DEPTH 32
#include "afade_template.c"

#undef DEPTH
#define DEPTH 64
#include "afade_template.c"

static int config_output(AVFilterLink *outlink)
{
    AVFilterContext *ctx = outlink->src;
    AudioFadeContext *s  = ctx->priv;

    switch (outlink->format) {
    case AV_SAMPLE_FMT_DBL:  s->fade_samples = fade_samples_dbl;
                             s->scale_samples = scale_samples_dbl;
                             break;
    case AV_SAMPLE_FMT_DBLP: s->fade_samples = fade_samplesp_dbl;
                             s->scale_samples = scale_samplesp_dbl;
                             break;
    case AV_SAMPLE_FMT_FLT:  s->fade_samples = fade_samples_flt;
                             s->scale_samples = scale_samples_flt;
                             break;
    case AV_SAMPLE_FMT_FLTP: s->fade_samples = fade_samplesp_flt;
                             s->scale_samples = scale_samplesp_flt;
                             break;
    case AV_SAMPLE_FMT_S16:  s->fade_samples = fade_samples_s16;
                             s->scale_samples = scale_samples_s16;
                             break;
    case AV_SAMPLE_FMT_S16P: s->fade_samples = fade_samplesp_s16;
                             s->scale_samples = scale_samplesp_s16;
                             break;
    case AV_SAMPLE_FMT_S32:  s->fade_samples = fade_samples_s32;
                             s->scale_samples = scale_samples_s32;
                             break;
    case AV_SAMPLE_FMT_S32P: s->fade_samples = fade_samplesp_s32;
                             s->scale_samples = scale_samplesp_s32;
                             break;
    default: return AVERROR_BUG;
    }

    if (s->duration)
        s->nb_samples = av_rescale(s->duration, outlink->sample_rate, AV_TIME_BASE);
    s->duration = 0;
    if (s->start_time)
        s->start_sample = av_rescale(s->start_time, outlink->sample_rate, AV_TIME_BASE);
    s->start_time = 0;

    return 0;
}

#if CONFIG_AFADE_FILTER

static const AVOption afade_options[] = {
    { "type",         "set the fade direction",                      OFFSET(type),         AV_OPT_TYPE_INT,    {.i64 = 0    }, 0, 1, TFLAGS, .unit = "type" },
    { "t",            "set the fade direction",                      OFFSET(type),         AV_OPT_TYPE_INT,    {.i64 = 0    }, 0, 1, TFLAGS, .unit = "type" },
    { "in",           "fade-in",                                     0,                    AV_OPT_TYPE_CONST,  {.i64 = 0    }, 0, 0, TFLAGS, .unit = "type" },
    { "out",          "fade-out",                                    0,                    AV_OPT_TYPE_CONST,  {.i64 = 1    }, 0, 0, TFLAGS, .unit = "type" },
    { "start_sample", "set number of first sample to start fading",  OFFSET(start_sample), AV_OPT_TYPE_INT64,  {.i64 = 0    }, 0, INT64_MAX, TFLAGS },
    { "ss",           "set number of first sample to start fading",  OFFSET(start_sample), AV_OPT_TYPE_INT64,  {.i64 = 0    }, 0, INT64_MAX, TFLAGS },
    { "nb_samples",   "set number of samples for fade duration",     OFFSET(nb_samples),   AV_OPT_TYPE_INT64,  {.i64 = 44100}, 1, INT64_MAX, TFLAGS },
    { "ns",           "set number of samples for fade duration",     OFFSET(nb_samples),   AV_OPT_TYPE_INT64,  {.i64 = 44100}, 1, INT64_MAX, TFLAGS },
    { "start_time",   "set time to start fading",                    OFFSET(start_time),   AV_OPT_TYPE_DURATION, {.i64 = 0 },  0, INT64_MAX, TFLAGS },
    { "st",           "set time to start fading",                    OFFSET(start_time),   AV_OPT_TYPE_DURATION, {.i64 = 0 },  0, INT64_MAX, TFLAGS },
    { "duration",     "set fade duration",                           OFFSET(duration),     AV_OPT_TYPE_DURATION, {.i64 = 0 },  0, INT64_MAX, TFLAGS },
    { "d",            "set fade duration",                           OFFSET(duration),     AV_OPT_TYPE_DURATION, {.i64 = 0 },  0, INT64_MAX, TFLAGS },
    { "curve",        "set fade curve type",                         OFFSET(curve),        AV_OPT_TYPE_INT,    {.i64 = TRI  }, NONE, NB_CURVES - 1, TFLAGS, .unit = "curve" },
    { "c",            "set fade curve type",                         OFFSET(curve),        AV_OPT_TYPE_INT,    {.i64 = TRI  }, NONE, NB_CURVES - 1, TFLAGS, .unit = "curve" },
    { "nofade",       "no fade; keep audio as-is",                   0,                    AV_OPT_TYPE_CONST,  {.i64 = NONE }, 0, 0, TFLAGS, .unit = "curve" },
    { "tri",          "linear slope",                                0,                    AV_OPT_TYPE_CONST,  {.i64 = TRI  }, 0, 0, TFLAGS, .unit = "curve" },
    { "qsin",         "quarter of sine wave",                        0,                    AV_OPT_TYPE_CONST,  {.i64 = QSIN }, 0, 0, TFLAGS, .unit = "curve" },
    { "esin",         "exponential sine wave",                       0,                    AV_OPT_TYPE_CONST,  {.i64 = ESIN }, 0, 0, TFLAGS, .unit = "curve" },
    { "hsin",         "half of sine wave",                           0,                    AV_OPT_TYPE_CONST,  {.i64 = HSIN }, 0, 0, TFLAGS, .unit = "curve" },
    { "log",          "logarithmic",                                 0,                    AV_OPT_TYPE_CONST,  {.i64 = LOG  }, 0, 0, TFLAGS, .unit = "curve" },
    { "ipar",         "inverted parabola",                           0,                    AV_OPT_TYPE_CONST,  {.i64 = IPAR }, 0, 0, TFLAGS, .unit = "curve" },
    { "qua",          "quadratic",                                   0,                    AV_OPT_TYPE_CONST,  {.i64 = QUA  }, 0, 0, TFLAGS, .unit = "curve" },
    { "cub",          "cubic",                                       0,                    AV_OPT_TYPE_CONST,  {.i64 = CUB  }, 0, 0, TFLAGS, .unit = "curve" },
    { "squ",          "square root",                                 0,                    AV_OPT_TYPE_CONST,  {.i64 = SQU  }, 0, 0, TFLAGS, .unit = "curve" },
    { "cbr",          "cubic root",                                  0,                    AV_OPT_TYPE_CONST,  {.i64 = CBR  }, 0, 0, TFLAGS, .unit = "curve" },
    { "par",          "parabola",                                    0,                    AV_OPT_TYPE_CONST,  {.i64 = PAR  }, 0, 0, TFLAGS, .unit = "curve" },
    { "exp",          "exponential",                                 0,                    AV_OPT_TYPE_CONST,  {.i64 = EXP  }, 0, 0, TFLAGS, .unit = "curve" },
    { "iqsin",        "inverted quarter of sine wave",               0,                    AV_OPT_TYPE_CONST,  {.i64 = IQSIN}, 0, 0, TFLAGS, .unit = "curve" },
    { "ihsin",        "inverted half of sine wave",                  0,                    AV_OPT_TYPE_CONST,  {.i64 = IHSIN}, 0, 0, TFLAGS, .unit = "curve" },
    { "dese",         "double-exponential seat",                     0,                    AV_OPT_TYPE_CONST,  {.i64 = DESE }, 0, 0, TFLAGS, .unit = "curve" },
    { "desi",         "double-exponential sigmoid",                  0,                    AV_OPT_TYPE_CONST,  {.i64 = DESI }, 0, 0, TFLAGS, .unit = "curve" },
    { "losi",         "logistic sigmoid",                            0,                    AV_OPT_TYPE_CONST,  {.i64 = LOSI }, 0, 0, TFLAGS, .unit = "curve" },
    { "sinc",         "sine cardinal function",                      0,                    AV_OPT_TYPE_CONST,  {.i64 = SINC }, 0, 0, TFLAGS, .unit = "curve" },
    { "isinc",        "inverted sine cardinal function",             0,                    AV_OPT_TYPE_CONST,  {.i64 = ISINC}, 0, 0, TFLAGS, .unit = "curve" },
    { "quat",         "quartic",                                     0,                    AV_OPT_TYPE_CONST,  {.i64 = QUAT }, 0, 0, TFLAGS, .unit = "curve" },
    { "quatr",        "quartic root",                                0,                    AV_OPT_TYPE_CONST,  {.i64 = QUATR}, 0, 0, TFLAGS, .unit = "curve" },
    { "qsin2",        "squared quarter of sine wave",                0,                    AV_OPT_TYPE_CONST,  {.i64 = QSIN2}, 0, 0, TFLAGS, .unit = "curve" },
    { "hsin2",        "squared half of sine wave",                   0,                    AV_OPT_TYPE_CONST,  {.i64 = HSIN2}, 0, 0, TFLAGS, .unit = "curve" },
    { "silence",      "set the silence gain",                        OFFSET(silence),      AV_OPT_TYPE_DOUBLE, {.dbl = 0 },    0, 1, TFLAGS },
    { "unity",        "set the unity gain",                          OFFSET(unity),        AV_OPT_TYPE_DOUBLE, {.dbl = 1 },    0, 1, TFLAGS },
    { NULL }
};

AVFILTER_DEFINE_CLASS(afade);

static av_cold int init(AVFilterContext *ctx)
{
    AudioFadeContext *s = ctx->priv;

    if (INT64_MAX - s->nb_samples < s->start_sample)
        return AVERROR(EINVAL);

    return 0;
}

static int filter_frame(AVFilterLink *inlink, AVFrame *buf)
{
    AVFilterContext *ctx = inlink->dst;
    AudioFadeContext *s = ctx->priv;
    AVFilterLink *outlink = ctx->outputs[0];
    int nb_samples          = buf->nb_samples;
    AVFrame *out_buf;
    int64_t cur_sample = av_rescale_q(buf->pts, inlink->time_base, (AVRational){1, inlink->sample_rate});

    if (s->unity == 1.0 &&
        ((!s->type && (s->start_sample + s->nb_samples < cur_sample)) ||
         ( s->type && (cur_sample + nb_samples < s->start_sample))))
        return ff_filter_frame(outlink, buf);

    if (av_frame_is_writable(buf)) {
        out_buf = buf;
    } else {
        out_buf = ff_get_audio_buffer(outlink, nb_samples);
        if (!out_buf)
            return AVERROR(ENOMEM);
        av_frame_copy_props(out_buf, buf);
    }

    if ((!s->type && (cur_sample + nb_samples < s->start_sample)) ||
        ( s->type && (s->start_sample + s->nb_samples < cur_sample))) {
        if (s->silence == 0.) {
            av_samples_set_silence(out_buf->extended_data, 0, nb_samples,
                                   out_buf->ch_layout.nb_channels, out_buf->format);
        } else {
            s->scale_samples(out_buf->extended_data, buf->extended_data,
                             nb_samples, buf->ch_layout.nb_channels,
                             s->silence);
        }
    } else if (( s->type && (cur_sample + nb_samples < s->start_sample)) ||
               (!s->type && (s->start_sample + s->nb_samples < cur_sample))) {
        s->scale_samples(out_buf->extended_data, buf->extended_data,
                         nb_samples, buf->ch_layout.nb_channels,
                         s->unity);
    } else {
        int64_t start;

        if (!s->type)
            start = cur_sample - s->start_sample;
        else
            start = s->start_sample + s->nb_samples - cur_sample;

        s->fade_samples(out_buf->extended_data, buf->extended_data,
                        nb_samples, buf->ch_layout.nb_channels,
                        s->type ? -1 : 1, start,
                        s->nb_samples, s->curve, s->silence, s->unity);
    }

    if (buf != out_buf)
        ff_graph_frame_free(ctx, &buf);

    return ff_filter_frame(outlink, out_buf);
}

static int process_command(AVFilterContext *ctx, const char *cmd, const char *arg)
{
    int ret = ff_filter_process_command(ctx, cmd, arg);

    if (ret < 0)
        return ret;

    return config_output(ctx->outputs[0]);
}

static const AVFilterPad afade_inputs[] = {
    {
        .name         = "default",
        .type         = AVMEDIA_TYPE_AUDIO,
        .filter_frame = filter_frame,
    },
};

static const AVFilterPad afade_outputs[] = {
    {
        .name         = "default",
        .type         = AVMEDIA_TYPE_AUDIO,
        .config_props = config_output,
    },
};

const FFFilter ff_af_afade = {
    .p.name        = "afade",
    .p.description = NULL_IF_CONFIG_SMALL("Fade in/out input audio."),
    .p.priv_class  = &afade_class,
    .priv_size     = sizeof(AudioFadeContext),
    .init          = init,
    FILTER_INPUTS(afade_inputs),
    FILTER_OUTPUTS(afade_outputs),
    FILTER_SAMPLEFMTS_ARRAY(sample_fmts),
    .process_command = process_command,
    .p.flags       = AVFILTER_FLAG_SUPPORT_TIMELINE_GENERIC,
};

#endif /* CONFIG_AFADE_FILTER */

#if CONFIG_ACROSSFADE_FILTER

static const AVOption acrossfade_options[] = {
    { "nb_samples",   "set number of samples for cross fade duration", OFFSET(nb_samples),   AV_OPT_TYPE_INT64,  {.i64 = 44100}, 1, INT_MAX, FLAGS },
    { "ns",           "set number of samples for cross fade duration", OFFSET(nb_samples),   AV_OPT_TYPE_INT64,  {.i64 = 44100}, 1, INT_MAX, FLAGS },
    { "duration",     "set cross fade duration",                       OFFSET(duration),     AV_OPT_TYPE_DURATION, {.i64 = 0 },  0, INT64_MAX/2, FLAGS },
    { "d",            "set cross fade duration",                       OFFSET(duration),     AV_OPT_TYPE_DURATION, {.i64 = 0 },  0, INT64_MAX/2, FLAGS },
    { "overlap",      "overlap 1st stream end with 2nd stream start",  OFFSET(overlap),      AV_OPT_TYPE_BOOL,   {.i64 = 1    }, 0,  1, FLAGS },
    { "o",            "overlap 1st stream end with 2nd stream start",  OFFSET(overlap),      AV_OPT_TYPE_BOOL,   {.i64 = 1    }, 0,  1, FLAGS },
    { "curve1",       "set fade curve type for 1st stream",            OFFSET(curve),        AV_OPT_TYPE_INT,    {.i64 = TRI  }, NONE, NB_CURVES - 1, FLAGS, .unit = "curve" },
    { "c1",           "set fade curve type for 1st stream",            OFFSET(curve),        AV_OPT_TYPE_INT,    {.i64 = TRI  }, NONE, NB_CURVES - 1, FLAGS, .unit = "curve" },
    {     "nofade",   "no fade; keep audio as-is",                     0,                    AV_OPT_TYPE_CONST,  {.i64 = NONE }, 0, 0, FLAGS, .unit = "curve" },
    {     "tri",      "linear slope",                                  0,                    AV_OPT_TYPE_CONST,  {.i64 = TRI  }, 0, 0, FLAGS, .unit = "curve" },
    {     "qsin",     "quarter of sine wave",                          0,                    AV_OPT_TYPE_CONST,  {.i64 = QSIN }, 0, 0, FLAGS, .unit = "curve" },
    {     "esin",     "exponential sine wave",                         0,                    AV_OPT_TYPE_CONST,  {.i64 = ESIN }, 0, 0, FLAGS, .unit = "curve" },
    {     "hsin",     "half of sine wave",                             0,                    AV_OPT_TYPE_CONST,  {.i64 = HSIN }, 0, 0, FLAGS, .unit = "curve" },
    {     "log",      "logarithmic",                                   0,                    AV_OPT_TYPE_CONST,  {.i64 = LOG  }, 0, 0, FLAGS, .unit = "curve" },
    {     "ipar",     "inverted parabola",                             0,                    AV_OPT_TYPE_CONST,  {.i64 = IPAR }, 0, 0, FLAGS, .unit = "curve" },
    {     "qua",      "quadratic",                                     0,                    AV_OPT_TYPE_CONST,  {.i64 = QUA  }, 0, 0, FLAGS, .unit = "curve" },
    {     "cub",      "cubic",                                         0,                    AV_OPT_TYPE_CONST,  {.i64 = CUB  }, 0, 0, FLAGS, .unit = "curve" },
    {     "squ",      "square root",                                   0,                    AV_OPT_TYPE_CONST,  {.i64 = SQU  }, 0, 0, FLAGS, .unit = "curve" },
    {     "cbr",      "cubic root",                                    0,                    AV_OPT_TYPE_CONST,  {.i64 = CBR  }, 0, 0, FLAGS, .unit = "curve" },
    {     "par",      "parabola",                                      0,                    AV_OPT_TYPE_CONST,  {.i64 = PAR  }, 0, 0, FLAGS, .unit = "curve" },
    {     "exp",      "exponential",                                   0,                    AV_OPT_TYPE_CONST,  {.i64 = EXP  }, 0, 0, FLAGS, .unit = "curve" },
    {     "iqsin",    "inverted quarter of sine wave",                 0,                    AV_OPT_TYPE_CONST,  {.i64 = IQSIN}, 0, 0, FLAGS, .unit = "curve" },
    {     "ihsin",    "inverted half of sine wave",                    0,                    AV_OPT_TYPE_CONST,  {.i64 = IHSIN}, 0, 0, FLAGS, .unit = "curve" },
    {     "dese",     "double-exponential seat",                       0,                    AV_OPT_TYPE_CONST,  {.i64 = DESE }, 0, 0, FLAGS, .unit = "curve" },
    {     "desi",     "double-exponential sigmoid",                    0,                    AV_OPT_TYPE_CONST,  {.i64 = DESI }, 0, 0, FLAGS, .unit = "curve" },
    {     "losi",     "logistic sigmoid",                              0,                    AV_OPT_TYPE_CONST,  {.i64 = LOSI }, 0, 0, FLAGS, .unit = "curve" },
    {     "sinc",     "sine cardinal function",                        0,                    AV_OPT_TYPE_CONST,  {.i64 = SINC }, 0, 0, FLAGS, .unit = "curve" },
    {     "isinc",    "inverted sine cardinal function",               0,                    AV_OPT_TYPE_CONST,  {.i64 = ISINC}, 0, 0, FLAGS, .unit = "curve" },
    {     "quat",     "quartic",                                       0,                    AV_OPT_TYPE_CONST,  {.i64 = QUAT }, 0, 0, FLAGS, .unit = "curve" },
    {     "quatr",    "quartic root",                                  0,                    AV_OPT_TYPE_CONST,  {.i64 = QUATR}, 0, 0, FLAGS, .unit = "curve" },
    {     "qsin2",    "squared quarter of sine wave",                  0,                    AV_OPT_TYPE_CONST,  {.i64 = QSIN2}, 0, 0, FLAGS, .unit = "curve" },
    {     "hsin2",    "squared half of sine wave",                     0,                    AV_OPT_TYPE_CONST,  {.i64 = HSIN2}, 0, 0, FLAGS, .unit = "curve" },
    { "curve2",       "set fade curve type for 2nd stream",            OFFSET(curve2),       AV_OPT_TYPE_INT,    {.i64 = TRI  }, NONE, NB_CURVES - 1, FLAGS, .unit = "curve" },
    { "c2",           "set fade curve type for 2nd stream",            OFFSET(curve2),       AV_OPT_TYPE_INT,    {.i64 = TRI  }, NONE, NB_CURVES - 1, FLAGS, .unit = "curve" },
    { NULL }
};

AVFILTER_DEFINE_CLASS(acrossfade);

/* Copy samples from frame to ring buffer (circular overwrite) */
static void copy_to_ring_buffer(AudioFadeContext *s, AVFrame *frame, int nb_channels, int is_planar)
{
    int bytes_per_sample = av_get_bytes_per_sample(frame->format);
    /* Clamp to ring buffer size; if frame is larger, keep only the last nb_samples */
    int samples_to_copy = FFMIN(frame->nb_samples, s->nb_samples);
    int src_offset = frame->nb_samples - samples_to_copy;
    int64_t dst_pos = s->ring_write_pos % s->nb_samples;
    int first_chunk = FFMIN(samples_to_copy, s->nb_samples - dst_pos);
    int second_chunk = samples_to_copy - first_chunk;

    if (is_planar) {
        for (int c = 0; c < nb_channels; c++) {
            memcpy(s->ring_buf->extended_data[c] + dst_pos * bytes_per_sample,
                   frame->extended_data[c] + src_offset * bytes_per_sample,
                   first_chunk * bytes_per_sample);
            if (second_chunk > 0)
                memcpy(s->ring_buf->extended_data[c],
                       frame->extended_data[c] + (src_offset + first_chunk) * bytes_per_sample,
                       second_chunk * bytes_per_sample);
        }
    } else {
        int stride = nb_channels * bytes_per_sample;
        memcpy(s->ring_buf->extended_data[0] + dst_pos * stride,
               frame->extended_data[0] + src_offset * stride,
               first_chunk * stride);
        if (second_chunk > 0)
            memcpy(s->ring_buf->extended_data[0],
                   frame->extended_data[0] + (src_offset + first_chunk) * stride,
                   second_chunk * stride);
    }

    s->ring_write_pos += samples_to_copy;
    s->ring_filled = FFMIN(s->ring_filled + samples_to_copy, s->nb_samples);
}

/* Read samples from ring buffer starting at crossfade_pos (circular read) */
static void read_from_ring_buffer(AudioFadeContext *s, uint8_t **dst, int nb_samples,
                                  int nb_channels, int is_planar, int bytes_per_sample)
{
    int64_t oldest_pos = (s->ring_write_pos - s->ring_filled + s->nb_samples) % s->nb_samples;
    int64_t read_start = (oldest_pos + s->crossfade_pos) % s->nb_samples;
    int first_chunk = FFMIN(nb_samples, s->nb_samples - read_start);
    int second_chunk = nb_samples - first_chunk;

    av_assert0(nb_samples <= s->ring_filled - s->crossfade_pos);

    if (is_planar) {
        for (int c = 0; c < nb_channels; c++) {
            memcpy(dst[c],
                   s->ring_buf->extended_data[c] + read_start * bytes_per_sample,
                   first_chunk * bytes_per_sample);
            if (second_chunk > 0)
                memcpy(dst[c] + first_chunk * bytes_per_sample,
                       s->ring_buf->extended_data[c],
                       second_chunk * bytes_per_sample);
        }
    } else {
        int stride = nb_channels * bytes_per_sample;
        memcpy(dst[0],
               s->ring_buf->extended_data[0] + read_start * stride,
               first_chunk * stride);
        if (second_chunk > 0)
            memcpy(dst[0] + first_chunk * stride,
                   s->ring_buf->extended_data[0],
                   second_chunk * stride);
    }
}

/* Process crossfade for non-overlap mode (fade-out then fade-in) */
static int process_non_overlap_crossfade(AVFilterContext *ctx, const int idx1)
{
    AudioFadeContext *s = ctx->priv;
    AVFilterLink *outlink = ctx->outputs[0];
    AVFilterLink *in1 = ctx->inputs[idx1];
    AVFrame *out, *cf = NULL;
    int ret;

    /* Phase 1: Fade-out from ring buffer */
    if (s->crossfade_pos < s->ring_filled) {
        int64_t remaining = s->ring_filled - s->crossfade_pos;
        int process_samples = FFMIN(remaining, 4096);
        int bytes_per_sample = av_get_bytes_per_sample(outlink->format);
        int is_planar = av_sample_fmt_is_planar(outlink->format);
        int nb_channels = outlink->ch_layout.nb_channels;

        out = ff_get_audio_buffer(outlink, process_samples);
        if (!out)
            return AVERROR(ENOMEM);

        AVFrame *temp = ff_get_audio_buffer(outlink, process_samples);
        if (!temp) {
            av_frame_free(&out);
            return AVERROR(ENOMEM);
        }

        read_from_ring_buffer(s, temp->extended_data, process_samples,
                              nb_channels, is_planar, bytes_per_sample);

        s->fade_samples(out->extended_data, temp->extended_data, process_samples,
                        nb_channels, -1, s->ring_filled - 1 - s->crossfade_pos,
                        s->ring_filled, s->curve, 0., 1.);

        s->crossfade_pos += process_samples;
        out->pts = s->pts;
        s->pts += av_rescale_q(process_samples,
            (AVRational){ 1, outlink->sample_rate }, outlink->time_base);
        av_frame_free(&temp);
        return ff_filter_frame(outlink, out);
    }

    /* Phase 2: Fade-in from input 1 */
    if (!ff_inlink_queued_samples(in1)) {
        int status;
        if (ff_inlink_acknowledge_status(in1, &status, NULL)) {
            s->crossfade_active = 0;
            s->passthrough = 0;
            ff_outlink_set_status(outlink, status, s->pts);
            return 0;
        }
        FF_FILTER_FORWARD_WANTED(outlink, in1);
        return FFERROR_NOT_READY;
    }

    ret = ff_inlink_consume_frame(in1, &cf);
    if (ret < 0)
        return ret;
    if (!ret) {
        FF_FILTER_FORWARD_WANTED(outlink, in1);
        return FFERROR_NOT_READY;
    }

    int64_t fadein_pos = s->crossfade_pos - s->ring_filled;
    int64_t fadein_remaining = s->ring_filled - fadein_pos;

    if (fadein_pos < s->ring_filled && fadein_remaining > 0) {
        int process_samples = FFMIN(cf->nb_samples, fadein_remaining);

        out = ff_get_audio_buffer(outlink, cf->nb_samples);
        if (!out) {
            av_frame_free(&cf);
            return AVERROR(ENOMEM);
        }

        s->fade_samples(out->extended_data, cf->extended_data, process_samples,
                        outlink->ch_layout.nb_channels, 1, fadein_pos,
                        s->ring_filled, s->curve2, 0., 1.);

        if (cf->nb_samples > process_samples) {
            int bytes_per_sample = av_get_bytes_per_sample(outlink->format);
            int is_planar = av_sample_fmt_is_planar(outlink->format);
            int nb_channels = outlink->ch_layout.nb_channels;

            if (is_planar) {
                for (int c = 0; c < nb_channels; c++) {
                    memcpy(out->extended_data[c] + process_samples * bytes_per_sample,
                           cf->extended_data[c] + process_samples * bytes_per_sample,
                           (cf->nb_samples - process_samples) * bytes_per_sample);
                }
            } else {
                memcpy(out->extended_data[0] + process_samples * nb_channels * bytes_per_sample,
                       cf->extended_data[0] + process_samples * nb_channels * bytes_per_sample,
                       (cf->nb_samples - process_samples) * nb_channels * bytes_per_sample);
            }
        }

        s->crossfade_pos += cf->nb_samples;
        out->pts = s->pts;
        s->pts += av_rescale_q(cf->nb_samples,
            (AVRational){ 1, outlink->sample_rate }, outlink->time_base);
        av_frame_free(&cf);

        if (s->crossfade_pos >= s->ring_filled * 2) {
            s->crossfade_active = 0;
        }

        return ff_filter_frame(outlink, out);
    }

    /* Past crossfade region - pass through */
    s->crossfade_active = 0;
    cf->pts = s->pts;
    s->pts += av_rescale_q(cf->nb_samples,
            (AVRational){ 1, outlink->sample_rate }, outlink->time_base);
    return ff_filter_frame(outlink, cf);
}

/* Process overlap crossfade incrementally from ring buffer and input 1 */
static int process_overlap_crossfade(AVFilterContext *ctx, const int idx1)
{
    AudioFadeContext *s = ctx->priv;
    AVFilterLink *outlink = ctx->outputs[0];
    AVFilterLink *in1 = ctx->inputs[idx1];
    int bytes_per_sample = av_get_bytes_per_sample(outlink->format);
    int is_planar = av_sample_fmt_is_planar(outlink->format);
    int nb_channels = outlink->ch_layout.nb_channels;
    AVFrame *out, *cf1 = NULL;
    int ret;

    int64_t remaining = s->ring_filled - s->crossfade_pos;
    if (remaining <= 0) {
        s->crossfade_active = 0;
        return 0;
    }

    if (!ff_inlink_queued_samples(in1)) {
        int status;
        if (ff_inlink_acknowledge_status(in1, &status, NULL)) {
            /* Input 1 ended early - fade out remaining ring buffer */
            int process_samples = FFMIN(remaining, 4096);

            out = ff_get_audio_buffer(outlink, process_samples);
            if (!out)
                return AVERROR(ENOMEM);

            AVFrame *temp = ff_get_audio_buffer(outlink, process_samples);
            if (!temp) {
                av_frame_free(&out);
                return AVERROR(ENOMEM);
            }

            read_from_ring_buffer(s, temp->extended_data, process_samples,
                                  nb_channels, is_planar, bytes_per_sample);

            s->fade_samples(out->extended_data, temp->extended_data, process_samples,
                            nb_channels, -1, s->ring_filled - 1 - s->crossfade_pos,
                            s->ring_filled, s->curve, 0., 1.);

            s->crossfade_pos += process_samples;
            out->pts = s->pts;
            s->pts += av_rescale_q(process_samples,
                (AVRational){ 1, outlink->sample_rate }, outlink->time_base);
            av_frame_free(&temp);

            if (s->crossfade_pos >= s->ring_filled)
                s->crossfade_active = 0;

            return ff_filter_frame(outlink, out);
        }
        FF_FILTER_FORWARD_WANTED(outlink, in1);
        return FFERROR_NOT_READY;
    }

    ret = ff_inlink_consume_frame(in1, &cf1);
    if (ret < 0)
        return ret;
    if (!ret) {
        FF_FILTER_FORWARD_WANTED(outlink, in1);
        return FFERROR_NOT_READY;
    }

    int process_samples = FFMIN(cf1->nb_samples, remaining);

    out = ff_get_audio_buffer(outlink, process_samples);
    if (!out) {
        av_frame_free(&cf1);
        return AVERROR(ENOMEM);
    }

    AVFrame *temp = ff_get_audio_buffer(outlink, process_samples);
    if (!temp) {
        av_frame_free(&out);
        av_frame_free(&cf1);
        return AVERROR(ENOMEM);
    }

    read_from_ring_buffer(s, temp->extended_data, process_samples,
                          nb_channels, is_planar, bytes_per_sample);

    s->crossfade_samples(out->extended_data, temp->extended_data,
                         cf1->extended_data, process_samples, nb_channels,
                         s->curve, s->curve2, s->crossfade_pos, s->ring_filled);

    s->crossfade_pos += process_samples;
    out->pts = s->pts;
    s->pts += av_rescale_q(process_samples,
        (AVRational){ 1, outlink->sample_rate }, outlink->time_base);

    av_frame_free(&temp);

    /* If we didn't use all of cf1, output the rest */
    if (cf1->nb_samples > process_samples) {
        AVFrame *remainder = ff_get_audio_buffer(outlink, cf1->nb_samples - process_samples);
        if (!remainder) {
            av_frame_free(&cf1);
            av_frame_free(&out);
            return AVERROR(ENOMEM);
        }
        if (is_planar) {
            for (int c = 0; c < nb_channels; c++) {
                memcpy(remainder->extended_data[c],
                       cf1->extended_data[c] + process_samples * bytes_per_sample,
                       (cf1->nb_samples - process_samples) * bytes_per_sample);
            }
        } else {
            memcpy(remainder->extended_data[0],
                   cf1->extended_data[0] + process_samples * nb_channels * bytes_per_sample,
                   (cf1->nb_samples - process_samples) * nb_channels * bytes_per_sample);
        }
        remainder->pts = s->pts;
        s->pts += av_rescale_q(cf1->nb_samples - process_samples,
            (AVRational){ 1, outlink->sample_rate }, outlink->time_base);
        av_frame_free(&cf1);
        s->crossfade_active = 0;
        ret = ff_filter_frame(outlink, out);
        if (ret < 0) {
            av_frame_free(&remainder);
            return ret;
        }
        return ff_filter_frame(outlink, remainder);
    }

    av_frame_free(&cf1);

    if (s->crossfade_pos >= s->ring_filled)
        s->crossfade_active = 0;

    return ff_filter_frame(outlink, out);
}

static int activate(AVFilterContext *ctx)
{
    AudioFadeContext *s = ctx->priv;
    AVFilterLink *outlink = ctx->outputs[0];
    const int idx1 = 1;
    int ret = 0;

    FF_FILTER_FORWARD_STATUS_BACK_ALL(outlink, ctx);

    /* If crossfade is active, process it */
    if (s->crossfade_active) {
        if (s->overlap)
            ret = process_overlap_crossfade(ctx, idx1);
        else
            ret = process_non_overlap_crossfade(ctx, idx1);

        if (ret < 0)
            return ret;

        if (!s->crossfade_active) {
            s->passthrough = 1;
        }
        return ret;
    }

    /* After crossfade: pass through input 1 */
    if (s->passthrough && s->status[0]) {
        if (ff_inlink_queued_frames(ctx->inputs[1])) {
            AVFrame *f = NULL;
            ret = ff_inlink_consume_frame(ctx->inputs[1], &f);
            if (ret < 0)
                return ret;
            if (f) {
                f->pts = s->pts;
                s->pts += av_rescale_q(f->nb_samples,
                    (AVRational){ 1, outlink->sample_rate }, outlink->time_base);
                return ff_filter_frame(outlink, f);
            }
        }
        FF_FILTER_FORWARD_STATUS(ctx->inputs[1], outlink);
        FF_FILTER_FORWARD_WANTED(outlink, ctx->inputs[1]);
    }

    /* Allocate ring buffer if needed */
    if (!s->ring_buf) {
        s->ring_buf = ff_get_audio_buffer(outlink, (int)s->nb_samples);
        if (!s->ring_buf)
            return AVERROR(ENOMEM);
    }

    /* Process input 0 */
    if (!s->status[0]) {
        AVFrame *frame = NULL;
        ret = ff_inlink_consume_frame(ctx->inputs[0], &frame);
        if (ret < 0)
            return ret;

        if (ret > 0) {
            int nb_channels = outlink->ch_layout.nb_channels;
            int is_planar = av_sample_fmt_is_planar(outlink->format);
            int bytes_per_sample = av_get_bytes_per_sample(outlink->format);

            if (s->overlap) {
                /* Overlap mode: delay output by nb_samples */
                int64_t total_received = s->ring_write_pos + frame->nb_samples;

                if (total_received <= s->nb_samples) {
                    /* Still filling the delay buffer */
                    copy_to_ring_buffer(s, frame, nb_channels, is_planar);
                    av_frame_free(&frame);
                    ff_inlink_request_frame(ctx->inputs[0]);
                    return 0;
                } else {
                    /* Have enough to start outputting */
                    int64_t excess = total_received - s->nb_samples;

                    if (s->ring_write_pos < s->nb_samples) {
                        /* Transition: ring buffer about to become full */
                        int64_t to_output = FFMIN(excess, s->ring_filled);

                        if (to_output <= 0) {
                            /* Nothing in ring yet (first frame larger than delay) */
                            copy_to_ring_buffer(s, frame, nb_channels, is_planar);
                            av_frame_free(&frame);
                            ff_inlink_request_frame(ctx->inputs[0]);
                            return 0;
                        }

                        AVFrame *out = ff_get_audio_buffer(outlink, (int)to_output);
                        if (!out) {
                            av_frame_free(&frame);
                            return AVERROR(ENOMEM);
                        }

                        /* Read oldest samples BEFORE writing (FIFO delay semantics) */
                        read_from_ring_buffer(s, out->extended_data, (int)to_output,
                                              nb_channels, is_planar, bytes_per_sample);

                        /* Now add entire new frame to ring */
                        copy_to_ring_buffer(s, frame, nb_channels, is_planar);

                        out->pts = s->pts;
                        s->pts += av_rescale_q((int)to_output,
                            (AVRational){ 1, outlink->sample_rate }, outlink->time_base);

                        av_frame_free(&frame);
                        return ff_filter_frame(outlink, out);
                    } else {
                        /* Steady state: output oldest from ring, add new to ring */
                        int from_ring = FFMIN(frame->nb_samples, s->ring_filled);
                        int from_frame = frame->nb_samples - from_ring;

                        AVFrame *out = ff_get_audio_buffer(outlink, frame->nb_samples);
                        if (!out) {
                            av_frame_free(&frame);
                            return AVERROR(ENOMEM);
                        }

                        /* Read delayed samples from ring */
                        if (from_ring > 0)
                            read_from_ring_buffer(s, out->extended_data, from_ring,
                                                  nb_channels, is_planar, bytes_per_sample);

                        /* Copy excess directly from new frame (beyond delay capacity) */
                        if (from_frame > 0) {
                            int offset = frame->nb_samples - from_frame;
                            if (is_planar) {
                                for (int c = 0; c < nb_channels; c++)
                                    memcpy(out->extended_data[c] + from_ring * bytes_per_sample,
                                           frame->extended_data[c] + offset * bytes_per_sample,
                                           from_frame * bytes_per_sample);
                            } else {
                                memcpy(out->extended_data[0] + from_ring * nb_channels * bytes_per_sample,
                                       frame->extended_data[0] + offset * nb_channels * bytes_per_sample,
                                       from_frame * nb_channels * bytes_per_sample);
                            }
                        }

                        copy_to_ring_buffer(s, frame, nb_channels, is_planar);

                        out->pts = s->pts;
                        s->pts += av_rescale_q(frame->nb_samples,
                            (AVRational){ 1, outlink->sample_rate }, outlink->time_base);

                        av_frame_free(&frame);
                        return ff_filter_frame(outlink, out);
                    }
                }
            } else {
                /* Non-overlap mode: pass through immediately, keep copy in ring buffer */
                copy_to_ring_buffer(s, frame, nb_channels, is_planar);

                frame->pts = s->pts;
                s->pts += av_rescale_q(frame->nb_samples,
                    (AVRational){ 1, outlink->sample_rate }, outlink->time_base);
                return ff_filter_frame(outlink, frame);
            }
        }

        if (ff_inlink_acknowledge_status(ctx->inputs[0], &s->status[0], NULL)) {
            /* Input 0 ended - start crossfade */
            s->crossfade_active = 1;
            s->crossfade_pos = 0;
            ff_filter_set_ready(ctx, 10);
            return 0;
        }

        if (ff_outlink_frame_wanted(outlink)) {
            ff_inlink_request_frame(ctx->inputs[0]);
            return 0;
        }
    }

    return FFERROR_NOT_READY;
}

static av_cold void uninit(AVFilterContext *ctx)
{
    AudioFadeContext *s = ctx->priv;
    av_frame_free(&s->ring_buf);
}

static int acrossfade_config_output(AVFilterLink *outlink)
{
    AVFilterContext *ctx = outlink->src;
    AudioFadeContext *s  = ctx->priv;

    outlink->time_base   = ctx->inputs[0]->time_base;

    switch (outlink->format) {
    case AV_SAMPLE_FMT_DBL:  s->crossfade_samples = crossfade_samples_dbl;  break;
    case AV_SAMPLE_FMT_DBLP: s->crossfade_samples = crossfade_samplesp_dbl; break;
    case AV_SAMPLE_FMT_FLT:  s->crossfade_samples = crossfade_samples_flt;  break;
    case AV_SAMPLE_FMT_FLTP: s->crossfade_samples = crossfade_samplesp_flt; break;
    case AV_SAMPLE_FMT_S16:  s->crossfade_samples = crossfade_samples_s16;  break;
    case AV_SAMPLE_FMT_S16P: s->crossfade_samples = crossfade_samplesp_s16; break;
    case AV_SAMPLE_FMT_S32:  s->crossfade_samples = crossfade_samples_s32;  break;
    case AV_SAMPLE_FMT_S32P: s->crossfade_samples = crossfade_samplesp_s32; break;
    default: return AVERROR_BUG;
    }

    config_output(outlink);

    if (s->nb_samples > INT_MAX) {
        av_log(ctx, AV_LOG_ERROR, "nb_samples %" PRId64 " exceeds maximum %d\n",
               s->nb_samples, INT_MAX);
        return AVERROR(EINVAL);
    }

    return 0;
}

static AVFrame *get_audio_buffer(AVFilterLink *inlink, int nb_samples)
{
    AVFilterContext *ctx = inlink->dst;
    AudioFadeContext *s = ctx->priv;

    return s->passthrough ?
        ff_null_get_audio_buffer   (inlink, nb_samples) :
        ff_default_get_audio_buffer(inlink, nb_samples);
}

static const AVFilterPad acrossfade_inputs[] = {
    {
        .name         = "crossfade0",
        .type         = AVMEDIA_TYPE_AUDIO,
        .get_buffer.audio = get_audio_buffer,
    },
    {
        .name         = "crossfade1",
        .type         = AVMEDIA_TYPE_AUDIO,
        .get_buffer.audio = get_audio_buffer,
    },
};

static const AVFilterPad acrossfade_outputs[] = {
    {
        .name          = "default",
        .type          = AVMEDIA_TYPE_AUDIO,
        .config_props  = acrossfade_config_output,
    },
};

const FFFilter ff_af_acrossfade = {
    .p.name        = "acrossfade",
    .p.description = NULL_IF_CONFIG_SMALL("Cross fade two input audio streams."),
    .p.priv_class  = &acrossfade_class,
    .priv_size     = sizeof(AudioFadeContext),
    .activate      = activate,
    .uninit        = uninit,
    FILTER_INPUTS(acrossfade_inputs),
    FILTER_OUTPUTS(acrossfade_outputs),
    FILTER_SAMPLEFMTS_ARRAY(sample_fmts),
};

#endif /* CONFIG_ACROSSFADE_FILTER */

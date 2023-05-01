#include "mc_visualizer.h"
#include "state.h"
#include "state.hpp"
#include "statespace.h"
#include <cmath>
#include <cstdlib>
#include <ctime>

#define R(c) ((c >> 16) & 0xff)
#define G(c) ((c >> 8) & 0xff)
#define B(c) ((c)&0xff)

#define make_rgb(r, g, b) ((r << 16) | (g << 8) | (b))
#define for_each_successors(s, f)                                                                                      \
  do {                                                                                                                 \
    int i, n = s->successor_num;                                                                                       \
    for (i = 0; i < n; i++) {                                                                                          \
      State *succ = state_succ_state(s, i);                                                                            \
      f(s, succ);                                                                                                      \
    }                                                                                                                  \
  } while (0)

int hsv2rgb(int h, int s, int v) {
  double ht = (double)h / 60;
  int    hi = floor(ht);
  double f  = ht - hi;

  double st = (double)s / 255;
  double m  = v * (1 - st);
  double n  = v * (1 - st * f);
  double k  = v * (1 - st * (1 - f));

  switch (hi) {
  case 0:
    return make_rgb((int)v, (int)k, (int)m);
  case 1:
    return make_rgb((int)n, (int)v, (int)m);
  case 2:
    return make_rgb((int)m, (int)v, (int)k);
  case 3:
    return make_rgb((int)m, (int)n, (int)v);
  case 4:
    return make_rgb((int)k, (int)m, (int)v);
  case 5:
    return make_rgb((int)v, (int)m, (int)n);
  }
  return 0;
}

void calc_colors(int worker_num, int **colors) {
  int i;
  int step = 360 / worker_num;

  for (i = 0; i < worker_num; i++) {
    (*colors)[i] = hsv2rgb(step * i, 150, 255);
  }
}

void dump_dot_state_edges(State *s) {
  int i, n;
  for (i = 0, n = s->successor_num; i < n; i++) {
    State *succ = state_succ_state(s, i);
    printf("  %lu -> %lu", state_hash(s), state_hash(succ));
    if (s->is_on_cycle() && succ->is_on_cycle())
      printf(" [color = \"#ff0000\"]");
    printf(";\n");
  }
}

void dump_dot_state_attr(State *s, AutomataRef *a, int *colors) {
  int color = s->is_expanded() ? colors[s->state_expander_id()] : 0x999999;
  printf("  %lu [label=\"", state_hash(s));
  if (s->is_expanded())
    printf("%lu", s->state_expander_id());
  printf("\", ");
  if (state_is_accept(*a, s))
    printf("peripheries = 2, ");
  if (s->is_on_cycle())
    printf("color = \"#ff0000\", ");
  printf("style = filled, fillcolor = \"#%02x%02x%02x\"", R(color), G(color), B(color));
  printf("];\n");
}

void dump_dot_header_comment(State *s) { printf("/* id: %lu   hash: %lu */\n", state_id(s), state_hash(s)); }

void dump_dot_loop(State *s, AutomataRef *a, int *colors, int depth) {
  int i, n;

  s->s_set_visited_by_visualizer();

  dump_dot_header_comment(s);
  dump_dot_state_edges(s);
  dump_dot_state_attr(s, a, colors);

  for (i = 0, n = s->successor_num; i < n; i++) {
    State *succ = state_succ_state(s, i);
    if (!succ->s_is_visited_by_visualizer()) {
      dump_dot_loop(succ, a, colors, depth + 1);
    }
  }
}

void dump_dot(StateSpaceRef ss, int worker_num) {
  int        *colors;
  State      *root = ss->initial_state();
  AutomataRef a    = ss->automata();

  colors           = (int *)malloc(sizeof(int) * worker_num);
  calc_colors(worker_num, &colors);

  printf("digraph statespace {\n");
  dump_dot_loop(root, &a, colors, 0);
  printf("}\n");

  free(colors);
}

extern "C"{
#include "mc_visualizer.h"
#include "statespace.h"
#include "state.h"
#include <stdlib.h>
#include <time.h>
#include <math.h>
}
#define R(c) ((c >> 16) & 0xff)
#define G(c) ((c >> 8) & 0xff)
#define B(c) ((c) & 0xff)

#define make_rgb(r, g, b) ((r << 16) | (g << 8) | (b))
#define for_each_successors(s, f)                 \
  do {                                          \
    int i, n = state_succ_num(s);             \
    for (i=0; i<n; i++) {                     \
      State* succ = state_succ_state(s, i); \
      f(s, succ);                           \
    }                                         \
  } while(0)                                    \

int hsv2rgb(int h, int s, int v) {
  double ht = (double)h / 60;
  int hi = floor(ht);
  double f = ht - hi;

  double st = (double)s / 255;
  double m = v * (1 - st);
  double n = v * (1 - st * f);
  double k = v * (1 - st * (1-f));

  switch (hi) {
    case 0: return make_rgb((int)v, (int)k, (int)m);
    case 1: return make_rgb((int)n, (int)v, (int)m);
    case 2: return make_rgb((int)m, (int)v, (int)k);
    case 3: return make_rgb((int)m, (int)n, (int)v);
    case 4: return make_rgb((int)k, (int)m, (int)v);
    case 5: return make_rgb((int)v, (int)m, (int)n);
  }
  return 0;
}

void calc_colors(int worker_num, int** colors) {
  int i, step = 360 / worker_num;

  for (i=0; i<worker_num; i++) {
    (*colors)[i] = hsv2rgb(step * i, 150, 255);
  }
}

void dump_dot_state_edges(State* s) {
  int i, n;
  for (i = 0, n = state_succ_num(s); i < n; i++) {
    State *succ = state_succ_state(s, i);
    printf("  %lu -> %lu", state_hash(s), state_hash(succ));
    if (is_on_cycle(s) && is_on_cycle(succ)) printf(" [color = \"#ff0000\"]");
    printf(";\n");
  }
}

void dump_dot_state_attr(State* s, AutomataRef* a, int* colors) {
  int color = is_expanded(s) ? colors[state_expander_id(s)] : 0x999999;
  printf("  %lu [label=\"", state_hash(s));
  if (is_expanded(s)) printf("%lu", state_expander_id(s));
  printf("\", ");
  if (state_is_accept(*a, s)) printf("peripheries = 2, ");
  if (is_on_cycle(s)) printf("color = \"#ff0000\", ");
  printf("style = filled, fillcolor = \"#%02x%02x%02x\"", R(color), G(color), B(color));
  printf("];\n");
}

void dump_dot_header_comment(State* s) {
  printf("/* id: %lu   hash: %lu */\n", state_id(s), state_hash(s));
}

void dump_dot_loop(State* s, AutomataRef* a, int* colors, int depth) {
  int i, n;

  s_set_visited_by_visualizer(s);

  dump_dot_header_comment(s);
  dump_dot_state_edges(s);
  dump_dot_state_attr(s, a, colors);

  for (i = 0, n = state_succ_num(s); i < n; i++) {
    State *succ = state_succ_state(s, i);
    if (!s_is_visited_by_visualizer(succ)) {
      dump_dot_loop(succ, a, colors, depth + 1);
    }
  }
}

void dump_dot(StateSpaceRef ss, int worker_num) {
  int* colors;
  State* root = statespace_init_state(ss);
  AutomataRef a = statespace_automata(ss);

  colors = (int*)malloc(sizeof(int) * worker_num);
  calc_colors(worker_num, &colors);

  printf("digraph statespace {\n");
  dump_dot_loop(root, &a, colors, 0);
  printf("}\n");

  free(colors);
}

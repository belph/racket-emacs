#include <stdlib.h>
#include <string.h>
#include <re-config.h>
#include <re-macros.h>

#include "hooks.h"
#include "emacs-c-utils.h"

typedef struct list_entry_s {
  re_hook_reginfo reginfo;
  struct list_entry_s *prev;
  struct list_entry_s *next;
} list_entry_t;

static list_entry_t *hooks[NUM_HOOKS] = { NULL };

static inline void print_reginfo(list_entry_t *entry) {
  dprintf("[entry: %p]: ", entry);
  dprintf("<reginfo: {type=%d; func=%p; dbg_name=%p}; prev=%p; next=%p>\n",
          entry->reginfo.type,
          entry->reginfo.func,
          &(entry->reginfo.dbg_name),
          entry->prev,
          entry->next);
}

int register_hook(re_hook_reginfo *rinfo) {
  list_entry_t *entry = malloc(sizeof(list_entry_t));
  if (entry == NULL) {
    return -1;
  }
  list_entry_t *last = hooks[rinfo->type];
  if (last != NULL) {
    while (last->next != NULL) {
      last = last->next;
    }
  }
  memcpy(&(entry->reginfo), rinfo, sizeof(re_hook_reginfo));
  entry->prev = last;
  entry->next = NULL;
  if (last) {
    last->next = entry;
  } else {
    hooks[rinfo->type] = entry;
  }
  dprintf("done\n");
  return 0;
}

int run_hooks(re_hook_type type, emacs_env *env) {
  dprintf("Running hooks for %d (env: %p)...\n", type, env);
  list_entry_t *entry = hooks[type];
  while (entry != NULL) {
    print_reginfo(entry);
    dprintf("Running hook: %s...\n", entry->reginfo.dbg_name);
    if (entry->reginfo.func(env)) {
      EMACS_CHECK_EXIT(env, -1);
      // No exception was thrown by the function; throw the default
      char buf[256] = { 0 };
      snprintf(buf, 255, "Failed to run hook: %s", entry->reginfo.dbg_name);
      EMACS_EXN_SLOW(env, "racket-emacs-hooks", buf);
      return -1;
    }
    dprintf("finished hook %s.\n", entry->reginfo.dbg_name);
    EMACS_CHECK_EXIT(env, -1);
    entry = entry->next;
  }
  return 0;
}

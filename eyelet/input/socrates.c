/*  Socrates is a mortal              */
/*  (Prolog → C translation, June 25) */

/*  ------------------------          */
/*  Original Prolog program           */
/*                                    */
/*  :- op(1200, xfx, :+).             */
/*                                    */
/*  :- dynamic(type/2).               */
/*                                    */
/*  type('Socrates', 'Man').          */
/*                                    */
/*  type(X, 'Mortal') :+              */
/*      type(X, 'Man').               */
/*                                    */
/*  % query                           */
/*  true :+ type(_, _).               */
/*  ------------------------          */

#include <stdio.h>
#include <string.h>
#include <stdbool.h>

/* a very small fixed-size “dynamic” database */
#define MAX_FACTS 16

typedef struct {
    const char *entity;     /* first argument  */
    const char *category;   /* second argument */
} Fact;

static Fact kb[MAX_FACTS];
static int fact_count = 0;

/* assertz/1 — store a fact if it is new                    */
static void assert_fact(const char *entity, const char *category)
{
    /* uniqueness guard (mimics Prolog’s set-like semantics here) */
    for (int i = 0; i < fact_count; ++i)
        if (!strcmp(kb[i].entity, entity) &&
            !strcmp(kb[i].category, category))
            return;

    if (fact_count < MAX_FACTS)
        kb[fact_count++] = (Fact){ entity, category };
}

/* rule: type(X, 'Mortal') :- type(X, 'Man').               */
static void apply_rules(void)
{
    /* single forward-chaining pass is enough for this toy rule */
    for (int i = 0; i < fact_count; ++i)
        if (!strcmp(kb[i].category, "Man"))
            assert_fact(kb[i].entity, "Mortal");
}

/* emulate the query true :+ type(_, _). — enumerate all solutions */
static void query_type_any_any(void)
{
    puts("Solutions to type(_, _):");
    for (int i = 0; i < fact_count; ++i)
        printf("  type('%s', '%s').\n", kb[i].entity, kb[i].category);
}

int main(void)
{
    /* dynamic(type/2) plus the base fact                     */
    assert_fact("Socrates", "Man");

    /* derive new facts via the rule                          */
    apply_rules();

    /* run the query                                          */
    query_type_any_any();

    return 0;
}


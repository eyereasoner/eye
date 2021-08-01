#include <stdio.h>

int main() {
    FILE *fptr;
    
    fptr = fopen("dt.pl","w");
    fprintf(fptr,"%% Deep taxonomy\n");
    fprintf(fptr,"%% See http://ruleml.org/WellnessRules/files/WellnessRulesN3-2009-11-10.pdf\n");
    fprintf(fptr,"\n");
    fprintf(fptr,"%% type/2 comes from http://www.w3.org/1999/02/22-rdf-syntax-ns#type\n");
    fprintf(fptr,"type(z,class(n0)).\n");
    for (int i = 0; i < 10000; i++) {
        fprintf(fptr,"type(X,class(n%d)) :- type(X,class(n%d)).\n",i+1,i);
        fprintf(fptr,"type(X,class(i%d)) :- type(X,class(n%d)).\n",i+1,i);
        fprintf(fptr,"type(X,class(j%d)) :- type(X,class(n%d)).\n",i+1,i);
    }
    fprintf(fptr,"\n");
    fprintf(fptr,"%% test cases\n");
    fprintf(fptr,"case(type(_,class(n1))).\n");
    fprintf(fptr,"case(type(_,class(n10))).\n");
    fprintf(fptr,"case(type(_,class(n100))).\n");
    fprintf(fptr,"case(type(_,class(n1000))).\n");
    fprintf(fptr,"case(type(_,class(n10000))).\n");
    fprintf(fptr,"\n");
    fprintf(fptr,"test :-\n");
    fprintf(fptr,"    case(A),\n");
    fprintf(fptr,"    A,\n");
    fprintf(fptr,"    write(A),\n");
    fprintf(fptr,"    write('.'),\n");
    fprintf(fptr,"    nl,\n");
    fprintf(fptr,"    fail.\n");
    fprintf(fptr,"test :-\n");
    fprintf(fptr,"    halt.\n");
    fclose(fptr);
    return 0;
}

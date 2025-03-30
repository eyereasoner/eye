# blogic

See https://www.slideshare.net/PatHayes/blogic-iswc-2009-invited-talk
and https://github.com/w3c-cg/rdfsurfaces

The top level surface is an implicit positive surface with implicit graffiti.

`log:onNegativeSurface` is a negative surface used to express NAND based logic:
- nand (not and) is a `log:onNegativeSurface`
- negation is a `log:onNegativeSurface`
- disjunction is a `log:onNegativeSurface` containing only `log:onNegativeSurface`'s
- `=>` is a `log:onNegativeSurface` containing a `log:onNegativeSurface`
- `<=` is a `log:onNegativeSurface` containing a `log:onNegativeSurface` of a `log:Component`
- query is a `log:onNegativeSurface` containing a `log:onNegativeAnswerSurface`

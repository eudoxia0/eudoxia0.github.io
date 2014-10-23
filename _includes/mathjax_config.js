MathJax.Hub.Config({
    "tex2jax": {"inlineMath": [['$','$'], ['\\(','\\)']]}
});
MathJax.Hub.Register.StartupHook("TeX Jax Ready",function () {
    MathJax.Hub.Insert(MathJax.InputJax.TeX.Definitions.macros,{
        cancel: ["Extension","cancel"],
    });
});

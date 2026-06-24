# What We Fixed — Plain-Language Summary

A one-sentence summary of each batch of fixes from the 2026-06 audit.

1. **Packaging** — We made the package pass R's official quality check cleanly
   so it's ready to submit to CRAN (declared all the libraries it depends on and
   fixed its description/metadata).

2. **Correctness** — We fixed several math/plotting bugs so that shaded regions
   under discrete probability distributions are drawn correctly (right groups,
   right endpoints included) and invalid inputs now warn instead of silently
   misbehaving.

3. **Documentation** — We corrected the help pages that described functions
   inaccurately (wrong titles, wrong parameter descriptions) so users reading
   the docs aren't misled.

4. **The paper** — We fixed factual errors in the R Journal paper (corrected
   numbers, equation names, and an abstract claim) and re-rendered it cleanly so
   the text, PDF, and all figures match.

5. **Tests** — We strengthened the test suite to actually check that the plots
   produce the *right values* (not just that they run), and removed a helper
   that was quietly adding stray columns to the data.

---
title: Methylphenidate and SSRIs
---

I take [methylphenidate][mph] (MPH) for [ADHD][adhd]. I've been thinking as of
late of adding an SSRI to help with my anxiety (possibly OCD? psychiatrists
unsure). But the [prescribing information][medsinfo] for MPH says:

> **Serotonin syndrome**
>
> Serotonin syndrome has been reported following co-administration of
> methylphenidate with serotonergic drugs such as selective serotonin reuptake
> inhibitors (SSRIs) and serotonin-norepinephrine reuptake inhibitors
> (SNRIs). The concomitant use of methylphenidate and serotonergic drugs is not
> recommended as this may lead to the development of serotonin syndrome. [...]

And, further down:

> **Use with serotonergic drugs**
>
> [...] Methylphenidate has been shown to increase extracellular serotonin and
> noradrenaline and appears to have weak potency in binding serotonin
> transporter.

Which is annoying! Analogously, the [drugs.com page][drugs] listing interactions between MPH and fluvoxamine (FLV) says:

> Methylphenidate may increase the blood levels and effects of fluvoxaMINE. You
> may be more likely to experience side effects, including a rare but serious
> condition called the serotonin syndrome, [...]

But it all sounds very vague. "Serotonin syndrome has been reported" could mean
there's a been a couple case reports, but there's a case report for everything
under the sun. What is the incidence? "Methylphenidate may increase the blood
levels of..." is also vague: why "may"?

# Case Reports

- metholodogy of finding case reports
- case report of SS

# Methylphenidate and Serotonin

Is methylphenidate serotonergic?

# Evidence for Safety

# Metabolism

## Substrates

|            | CES1 | CYP1A2          | CYP2B6            | CYP2C9            | CYP2C19           | CYP2D6                                | CYP3A4            |
|------------|------|-------------------|-------------------|-------------------|-------------------|---------------------------------------|-------------------|
| MPH        | Yes  | No[^Girdwood2021] | No[^Girdwood2021] | No[^Girdwood2021] | No[^Girdwood2021] | No[^DeVane2000] and no[^Girdwood2021] | No[^Girdwood2021] |
| FLV        | ?    | Yes               | ?                 | ?                 | Yes               | Yes                                   | Yes               |
| Sertraline | ?    | ?                 | Yes[^Obach2005]   | Yes[^Obach2005]   | Yes[^Obach2005]   | Yes[^Obach2005]                       | Yes[^Obach2005]   |

## Cytochrome Inhibition

Some drugs bind to P450 enzymes, inhibiting their metabolic activity.

|     | CES1 | CYP1A2 | CYP2B6           | CYP2C9 | CYP2C19 | CYP2D6          | CYP3A4 |
|-----|------|--------|------------------|--------|---------|-----------------|--------|
| MPH | ?    | ?      | Yes?[^Baird2003] |        | ?       | Yes[^Cozza2001] | ?      |
| FLV |      | Yes    | Yes              | Yes    | Yes     | Yes             | Yes    |


## Qualitative

### Effect of FLV on MPH

MPH is metabolized by CES1, which is not inhibited by FLV, therefore, the
metabolism of MPH is not affected by FLV.

### Effect of MPH on FLV

But MPH does inhibit CYP2D6, which is the primary metabolic pathway of
FLV. Therefore, MPH is expected to increase plasma concentrations of FLV.

[medsinfo]: https://medsinfo.com.au/product-information/document/Ritalin_PI?drug_id=329&documenttype=pi
[mph]: https://en.wikipedia.org/wiki/Methylphenidate
[adhd]: /article/notes-on-managing-adhd

# Footnotes

[^DeVane2000]:
    [PubMed](https://pubmed.ncbi.nlm.nih.gov/10831022/).

[^Baird2003]:
    [PDF](https://journals.sagepub.com/doi/pdf/10.1177/070674370304800615).

[^Girdwood2021]:
    _Perspectives from the Society for Pediatric Research: Pharmacogenetics for Pediatricians_, 2022. [PubMed](https://pmc.ncbi.nlm.nih.gov/articles/PMC8492778/). Note in particular: "Methylphenidate is not metabolized significantly by any CYP450 enzymes."

[^Obach2005]:
    [PubMed](https://pubmed.ncbi.nlm.nih.gov/15547048/). See: "Using P450
    isoform-selective inhibitors and recombinant heterologously expressed
    enzymes, it was demonstrated that several P450 enzymes catalyzed sertraline
    N-demethylation, with CYP2B6 contributing the greatest extent, and lesser
    contributions from CYP2C19, CYP2C9, CYP3A4, and CYP2D6."

[^Cozza2001]:
    _Concise Guide to the Cytochrome P450 System for Psychiatrists_. 2001. p 45. [archive.org](https://archive.org/details/conciseguidetocy0000cozz/page/45/mode/2up).

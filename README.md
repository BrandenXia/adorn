# Adorn

Minimal static site generation with Typst and Mustache.

```text
articles/(.*)\.typ => typst template=article.mustache => articles/$1.html
fragments/(.*)\.typ => typst bodyOnly=true => fragments/$1.html
papers/(.*)\.typ => typstPDF => papers/$1.pdf
assets/(.*) => copy => assets/$1
pages/(.*)\.html => compose => $1.html
private/.* => ignore

notes/(.*)\.typ => choose script=choice.sh {
  html: typst => notes/$1.html;
  pdf: typstPDF => notes/$1.pdf;
  skip: ignore;
}

docs/(.*)\.typ => check script=check-compatible.sh {
  yes: typst => docs/$1.html;
  no: typstPDF => docs/$1.pdf;
}

index template=index.mustache selection=articles/.* => index.html
```

# Adorn

Minimal static site generation with Typst and Mustache.

```haskell
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

index template=articles.mustache selection=articles/.* => articles.html

index template=index.mustache {
  latest: selection=articles/.* sort=meta.date dateFormat="%b %e, %Y" order=desc limit=5;
  articles: selection=articles/.* filter="meta.draft != true" sort=meta.date dateFormat="%b %e, %Y" order=desc;
  math: selection=articles/.* filter="'math' in meta.tags" sort=title order=asc;
  papers: selection=papers/.* sort=source;
} => index.html
```

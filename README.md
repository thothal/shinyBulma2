# shinyBulma

## Preface

This project is inspired by this repo [shinybulma](https://github.com/RinteRface/shinybulma)
and is yet another attempt to expose the wonderful [bulma CSS framework](https://bulma.io)
to shiny.

While the original shinybulma repo provides all of the bulma elements, containers
and addons, it lacks here and there the possibility to change the look and
feel of some components. While using it in production, I had to resort to 
`htmltools::tagAppendAttributes` regularly. In addition, some of the Javascript
code bundled with it would need some general overhaul as it seems to stem from
different sources and lacks some functionality. Eventually a connection to
the internet is needed to use the library, as not all CSS files are bundeled
with it.

Thus, I decided to give it a go and try to implement the bulma framework and 
some of the extensions from scratch. This is also an excellent opportunity to
further learn about modern Javascript tools, which I will use in this project.

## Installation

```
devtools::install_github("https://github.com/thothal/shinyBulma")
```

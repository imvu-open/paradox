# ChangeLog

## 1.12.0.0 (Next)

Release notes:
* Removed some memoization to ease concurrency
* Fix backend divergence of globbed counters and non globbed counters
* Add log function

Major changes:

Behavior changes:

Other enhancements:

Bug fixes:

## 1.11.0.0

Release notes:
* GHC 8.0.2
* lts-8.0

Major changes:

Behavior changes:

Other enhancements:

Bug fixes:

## 1.10.5.2

Other enhancements:
* Map fold'WithKey -> foldlWithKey', strictness
* Code cleanup in Paradox.Eval
* Update README.md

Bug fixes:
* fix scroll bar on function help dialog

## 1.10.5.1

Other enhancements:
* foldl' and foldl1'
* CONTRIBUTING.md added

## 1.10.5.0

Release notes:
* Fix broken scrollbars for dashboards and counter list on at least modern chrome and firefox installs

## 1.10.4.1

Other enhancements:
* Use ApplicativeDo for main grammar parser

## 1.10.4.0

Release notes:
* Relax even more whitespace sensitivity
  * Parens can now safely have whitespace on their concave side
  * The line should be able to have trailing and preceding whitespace

## 1.10.3.0

Other enhancements:
* Updated to megaparsec from parsec. Supposed to be faster and more flexible out of the box. Better errors

## 1.10.2.0

Release notes:
* Relax some whitespace sensitivity
  * Note: Parens must still not have whitespace on their concave side

## 1.10.1.0

Bug fixes:
* Javascript fixes to multiple templates in list and multiple showing of templates

## 1.10.0.0

Release notes:

* Currently parametrized dashboards (templates) must be configured via a text editor (no UX support)
* Config now requires a new key `template_dir`

Major changes:

* Parametrized dashboards added. They appear as another section under user and global dashboards.
* When selecting them they will ask you for the substition values
* A new topbar allows them to be applied without reloading
* loading from links also works
  * url form is /#?template=${TEMPLATE_NAME}&${REPLACEMENT_KEY1}=${JSON_ENCODED_REPLACEMENT_VALUE1}&${REPLACEMENT_KEY2}=${JSON_ENCODED_REPLACEMENT_VALUE2}
    * ${JSON_ENCODED_REPLACEMENT_VALUE} have the form
```javascript
{
  "key":"{KEY_OF_REPLACEMENT}",
  "replacer":"{REPLACEMENT_STRING}",
  "value":"{VALUE TO REPLACE WITH}"
}
```

* Server side, there is one template per file, and the filename must match the template name.
  * Templates are specified with the following json skeleton
```javascript
${TEMPLATE_DIR}/${TEMPLATE_NAME}
{
  "name":"${TEMPLATE_NAME}",
  "template":
    "[[\"ctr1.{SUB1}\",\"ctr2.{SUB1}\"\"],[\"ctr.in.second.graph\"],[\"ctr.in.thirdgraph\"],[\"ctr.in.fourthgraph.{SUB2}\",\"ctr.in.fourthgraph.{SUB2}\"]]",
  "replacements": {
    "sub1_is_a_type_of_host" : "{SUB1}",
    "sub2_is_an_attribute" : "{SUB2}"
    }
}
```
  * Note that the replacement key must be a valid js/html identifier
  * Note that the substition regex will be passed to RegExp(subst, 'g')
  * Note that the template may evolve in the future since that syntax is brutal

Behavior changes:

Other enhancements:

Bug fixes:

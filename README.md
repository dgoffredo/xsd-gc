`xsd-gc`
========
Remove unused types from a schema.

Why
---
Some service schemas are grotesquely large. If you're only going to be making
one request, might as well remove all of the other request and response types.
Don't forget to add the appropriate `id` attributes, if necessary.

What
----
`xsd-gc` is a Racket package that provides a command line tool for running
garbage collection on the types defined with a specified 
[XML Schema Definition](https://www.w3.org/TR/xmlschema-1/) (XSD) file.

How
---
### Usage example

```console
$ cat service.xsd
```
```xml
<TODO />
```
```console
$ raco xsd-gc service.xsd
```
```xml
<TODO />
```

### Install
```console
$ raco pkg install
```
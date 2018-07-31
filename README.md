![xsd-gc](xsd-gc.png)

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
<?xml version="1.0" encoding="UTF-8"?>
<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
           xmlns:ext="http://www.foo.com/extensions"
           ext:requestType="SomeChoice"
           ext:responseType="SomeChoice">

  <xs:complexType name="SomeChoice">
    <xs:choice>
      <xs:element name="foo"   type="xs:decimal"/>
      <xs:element name="bar"   type="xs:string"/>
      <xs:element name="baz"   type="Color"/>
      <xs:element name="water" type="WaterType"/>
    </xs:choice>
  </xs:complexType>

  <xs:simpleType name="Color">
    <xs:restriction base="xs:string">
      <xs:enumeration value="RED"   ext:id="0"/>
      <xs:enumeration value="GREEN" ext:id="1"/>
      <xs:enumeration value="BLUE"  ext:id="2"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:complexType name="Fish">
    <xs:sequence>
      <xs:element name="kind" type="WaterType"/>
      <xs:element name="name" type="xs:string"/>
    </xs:sequence>
  </xs:complexType>

  <xs:simpleType name="WaterType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="FRESH" ext:id="0"/>
      <xs:enumeration value="SALT"  ext:id="1"/>
    </xs:restriction>
  </xs:simpleType>

</xs:schema>
```
```console
$ raco xsd-gc service.xsd
```
```xml
<?xml version="1.0" encoding="UTF-8"?>
<xs:schema ext:requestType="SomeChoice" ext:responseType="SomeChoice" xmlns:ext="http://www.foo.com/extensions" xmlns:xs="http://www.w3.org/2001/XMLSchema">

  <xs:complexType name="SomeChoice">
    <xs:choice>
      <xs:element name="foo" type="xs:decimal"></xs:element>
      <xs:element name="bar" type="xs:string"></xs:element>
      <xs:element name="baz" type="Color"></xs:element>
      <xs:element name="water" type="WaterType"></xs:element>
    </xs:choice>
  </xs:complexType>

  <xs:simpleType name="Color">
    <xs:restriction base="xs:string">
      <xs:enumeration ext:id="0" value="RED"></xs:enumeration>
      <xs:enumeration ext:id="1" value="GREEN"></xs:enumeration>
      <xs:enumeration ext:id="2" value="BLUE"></xs:enumeration>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="WaterType">
    <xs:restriction base="xs:string">
      <xs:enumeration ext:id="0" value="FRESH"></xs:enumeration>
      <xs:enumeration ext:id="1" value="SALT"></xs:enumeration>
    </xs:restriction>
  </xs:simpleType>

</xs:schema>
```
No more fish.

### Install
```console
$ raco pkg install
```

More
----
### Namespaces
To keep the code simple, `xsd-gc` ignores all namespaces. This means that if
a schema refers to types in different namespaces that otherwise have the same
name, or if it deals with tags from outside the XSD namespace but that have
names like "complexType," `xsd-gc` might misbehave.

I might revisit this in the future (since the facilities for dealing with
namespaces in the SXML library are mostly sufficient).
# xmldata - A Generator for XML Data Conversion

This package contains a tool `curry-data2xml` with generates
for a given Curry module a new Curry module containing conversion functions
from and to an XML representation for all data types declared
in this module.

For instance, if `Nat` is a module containing the declaration

    data Nat = Z | S Nat

applying this program to `Nat` generates a new module `NatDataToXml`
containing the implementation of the following operations:

    natToXml :: Nat -> XmlExp
    xmlToNat :: XmlExp -> Nat

Hence, one can store a `Nat` term `num` into the file `Nat.xml` by

    writeXmlFile "Nat.xml" (natToXml num)

provided that the module `XML` is imported. Similarly, one can read
the data from this file by

    readXmlFile "Nat.xml" >>= return . xmlToNat


## Installing the tool

In order to install the current version of the tool, execute
the commands

    > cypm update
    > cypm install xmldata

This installs the executable `curry-data2xml` in the bin directory
of CPM.


## Using the tool

Execute the tool with the module containing the data for which
the conversion functions should be created, e.g.,

    > curry-data2xml Nat


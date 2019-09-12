# Darwin

This project is intended to represent a functional programming language which compiles to JVM bytecode. It's written in Haskell, and includes:

- A compiler which compiles our experimental functional language code into what we call "components."
- An API which converts the components (which are intermediate building blocks of the code) into JVM bytecode.

Why is the code converted into components instead of JVM directly? Well, it provides a level of abstraction, and also makes it easier to extend the language.

# Running the Compiled Programs

JVM, which stands for Java Virtual Machine, will be the host on which the programs (compiled by our compiler) will run. The bytecode will be generated and fed directly to JVM, no different from how typical Java code is compiled to bytecode.

Bytecode is pretty much similar to assembly code, except here the code we're going to write is going to be understandable only by JVM. Even there are CPUs which can natively run JVM bytecode! It's called bytecode because the code is composed of a series of bytes (8 bits).

Let's look at how JVM bytecode looks like:

```
cafe babe 0000 0034 0013 0700 0201 000a
4865 6c6c 6f57 6f72 6c64 0700 0401 0010
6a61 7661 2f6c 616e 672f 4f62 6a65 6374
0700 0601 0010 6a61 7661 2f6c 616e 672f
5379 7374 656d 0700 0801 0013 6a61 7661
2f69 6f2f 5072 696e 7453 7472 6561 6d08
000a 0100 0b48 656c 6c6f 2057 6f72 6c64
0900 0500 0c0c 000d 000e 0100 036f 7574
0100 154c 6a61 7661 2f69 6f2f 5072 696e
7453 7472 6561 6d3b 0a00 0700 100c 0011
0012 0100 0770 7269 6e74 6c6e 0100 1528
4c6a 6176 612f 6c61 6e67 2f53 7472 696e
673b 2956 0021 0001 0003 0000 0000 0000
0000
```

The bytecode is nothing more than 0s and 1s (binary). However, the bytecodes above are presented with hexadecimal encoding, which leads to characters from 0-9 and A-F (base 16). Hexadecimal encoding is just to make it a bit easier to read and write for us humans, nothing more.

Please beware that JVM is designed to run object oriented code primarily; so even if we want to write a "Hello World" program, there must be atleast a class and a method. Let's learn how to write a simple "Hello World" program in bytecode. This will help us understand a lot about how JVM bytecode is structured.

Let's begin with this Java code (just for purposes of understanding):

```java

public class HelloWorld {
    public static void main(String[] args) {
        System.out.println("Hello World!");
    }
}

```

To represent this code in bytecode, we need to make a class file. To see the structure of a class file written in bytecode, let's look at [JVM's specifications of one](https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html).
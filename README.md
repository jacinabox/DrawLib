A somewhat minimalist drawing library in pure Haskell. It contains really only the most basic components of a graphics library. It includes a Bezier spline drawing algorithm. My main motivation for writing it was that  I noticed that few libraries support anti-aliasing in pure software, preferring to export that functionality to a graphics card renderer; so this anti-aliasing algorithm works wihtout hardware support. It is currently Windows-only.

Here's a sample:

![](./Test.bmp)

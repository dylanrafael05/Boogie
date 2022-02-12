```lua
--function calls: () syntax
a + b()
a + (b()) --tightest binding

--function calls: f arg syntax
a + b c
a + (b c)
a + b c d
a + (b of (c d))

```
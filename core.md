```lua
--Comment
--*-Multiline long comment-*--

--Statements can either end with a line break or a semicolon
t1 := 0
t2 := 0;

-----------------------------------
-- Variables
-----------------------------------
let my_var --defines a variable (defaults to nil)
let my_other_var = 0 --sets a variable

--'let x' is roughly equivalent to
    --'x := nil'
--but can be used in more complex situations

-----------------------------------
-- Nil
-----------------------------------
--variables are defined inline with ':='
var := nil

-----------------------------------
-- Numbers
-----------------------------------
n := 0
n = infinity
n = nan

--seperators
n = 1`000`000

--binary and hex literals (future)
n = b`00001
n = x`0000F

-----------------------------------
-- Booleans
-----------------------------------
bool := true
bool = false

-----------------------------------
-- Strings: immutable
-----------------------------------
s := 'wow'
s = '''
    wow 2:
    multiline mode
'''

-----------------------------------
-- Lists: pass by reference
-----------------------------------
a := 0, 1, 2 --comma notation
a = (0, 1, 2) --parenthesized notation
a = [0, 1, 2] --list expression
a = [0] --list of one element
a = [] --empty list

a = 0 to 10 --range expression (0, 1, 2... 10)
a = 10 to 0 by -1 --range expression with 'by' (10, 9... 0)

-----------------------------------
-- Tables: pass by reference
-----------------------------------
b := {}

--Because scopes are themselves tables, table values are also set with ':='
b["x"] := 0 
b.y := 1

--Tables may also be constructed in-line
b = {["x"] := 0, y := 1}

-----------------------------------
-- Objects: frozen table
-----------------------------------
--future feature
-*--[[
obj := object obj in --block definition
    --'object' refers to the object being created
    obj.a := 0
    obj.b := 0
    obj.c := 0
    obj[0] := 0 --error! keys must be strings for an object
end
]]--*-

obj.d := 0 --error! cannot create new values of an object once created

obj = object {} --object expression

-----------------------------------
-- Vectors: pass by value
-----------------------------------
c := |0, 1, 2|

-----------------------------------
-- Functions
-----------------------------------
--Full form
d := function(a, b) do
    return a
end

--Lambda form
d := function(a) := a

--Simplified form
d(a) := do
    return a
end

--Simplified lambda form
d(a) := a

let result
result = d(0)
--Secondary calling syntax
result = d of 0
--Ternary calling syntax... yeah
result = 0 |> d

-----------------------------------
-- Future feature
-- Unpacking
-----------------------------------
minmax := 0, 2

let min
let max

min, max = minmax --unpacking to old vals
min2, max2 := minmax --unpaxking to new vars
min2, let min3 = minmax --unpaxking to new and old vars

-----------------------------------
-- Basic logic
-----------------------------------
basic() := do

    --Lexical scoping: this 'a' shadows global 'a'
    a := 0

    --Scopes can be accessed with 
        --globals
        --uppers
        --locals
    
    if a == 0 do
        --Simple if test
    end

    if a == 0 do
        nil
    elseif a == 1 do
        --elseif
    end

    --------------------------------------------
    --Future feature
    --If expressions; must have an 'else'
    --------------------------------------------
    val := if a == 0 then 1 else -1 end
    val =  if a == 0 then 1 elseif a == 1 then 2 end

    while a > 10 do
        --Loop until condition
    end

    for i in 0, 1, 2, 3, 4 do
        --Iterate over items in iterable
        --i is scoped to this block
    end

    for i in 1 to 20 do
        --Simple numerical for loop
    end

    --Line endings are significant; use '..' to denote a continuation
    a = 10 ..
        + 20
    
    --------------------------------------------
    --Future feature
    --Exceptions: use the 'throw' keyword 
    --to throw an object as an exception
    --------------------------------------------
    throw {code := 100, message := 'wow you suck'}

    --------------------------------------------
    --Future feature
    --Use the 'try' keyword to run with catching
    --Use the 'catch' keyword to catch exceptions
    --------------------------------------------
    try
        throw {code := 100, message := 'wow you suck'}
    catch err in
        throw --rethrow
    else

    end

end

--------------------------------------------
-- Methods
--------------------------------------------
e := object {
    x := 0,
    set_x(self, new_x) := do
        --note that when 'self' is included, its valued are considered in scope
        x = new_x --this is actually self.x
    end
}

e.set_a e 0
--Using ':' as an accessor will implictly use the lhs as the 'self' argument to the function called
e:set_a 0
--This will (throw an error - future) (crash - present) if the item is not a function

--------------------------------------------
-- Simple operations
--------------------------------------------

--Arithmetic
n = n + 1
n = n - 1
n = n * 2
n = n / 2
n = n % 2
n = n ** 2
n = n // 2
n = -n
n = abs n
n = floor n
n = round n
n = ceil n
n = n bit_or 1
n = n bit_and 1
n = bit_not n

--Booleans
b = b or b
b = b and b
b = not b
b = b == b
b = b ~= b
b = b > b
b = b < b
b = b >= b
b = b <= b

--Strings
s = s + ' concatenation'
s_len := len of s

--Lists
a = a + (0, 1, 2)
a_len := len of a
a_clone := a:clone()
a_deepclone := a:deepclone()
a:remove(0)
a:add(|0, 1|)
a:insert(0 |0, 1|)

--Tables
b_len := len of b
b_clone := b:clone()
b_deepclone := b:deepclone()
b:remove('x')
b:add('x', 0)

--Vectors
c = c + |0, 1, 2|
c = c - |0, 1, 2|
c = c * 2
c = c / 2
c = c *. |0, 1, 2| --dot product
c = c */ |0, 1, 2| --cross product
c = -c
c_mag := abs c
c_len := len c
c_x := c.x
c_y := c.y

--------------------------------------------
-- 'meta'
--------------------------------------------
--All keys in a table's (or object's) 'meta' are treated as if they existed inside the actual table.
--This table may contain metafunctions
    --A metafunction is simply a function which describes a basic behaviour like addition or abs

--------------------------------------------
-- Future feature
-- Variable arguments
--------------------------------------------
--Any function with a '...' as its last argument has variable arguments
--These functions return a value of 'infinity' for '.parameter_count'
f(...) := do
    vars := ... --A list
end

--------------------------------------------
-- Do blocks and scoping
--------------------------------------------
do
    inner_var := 0
end

inner_var = 0 --error!

--------------------------------------------
-- Table Modifiers
--------------------------------------------
a1 final := 0 --unmodifiable

--... equivalent to...
    -- a1 := 0
    -- globals:markFinal 'a1'

--------------------------------------------
-- Types and 'is'
--------------------------------------------
type Point(x, y) do
    self.x := x
    self.y := y
end

--the above is equivalent to
Point := {
    __meta := {
        __call(x, y) := object {
            x := x,
            y := y,
            meta := Point,
        }
    },
    __typename := 'Point'
}
--

-- Usage of the type: defining methods
Point.__add(self, other) := Point(x + other.x, y + other.y)

-- Usage of the type: construction
origin := Point(0, 0)

-- Using 'is' to test if something is an instance
print(origin is Point) --true

--inheritance
type Point3D(x, y, z) from Point(x, y) do
    self.z := z
end

--------------------------------------------
-- Metafunctions
--------------------------------------------

-- Arithmetic:
    -- __add(o)
    -- __sub(o)
    -- __mul(o)
    -- __div(o)
    -- __idiv(o)
    -- __mod(o)
    -- __pow(o)
    -- __dot_prod(o)
    -- __cross_prod(o)
    -- __neg()

    -- __abs()
    -- __floor()
    -- __round()
    -- __ceil()
    -- __sqrt()

    -- __bitand()
    -- __bitor()
    -- __bitnot()
    -- __bitxor()
    -- __shl()
    -- __shr()

-- Relational:
    -- __eq(o)
    -- __lt(o)
    -- __le(o) (can be omitted if __eq and __lt exist)

-- Indexing:
    -- __get(k)
    -- __set(k, v)
    -- __set_new(k, v)
    -- __len()
    -- __iter()
        -- must return an iterator, where an
        -- iterator is defined as a table (or object)
        -- with the methods 'advance', 'get', and 'reset'
    -- __unpack() -> list

-- General:
    -- __to_str()
    -- __to_int()
    -- __to_num()
    -- __to_bool()

-- Functional:
    -- __call(...)
        -- .parameter_count() will be derived from this
```
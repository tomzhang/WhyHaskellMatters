# Why Haskell Matters

[![Actions Status](https://github.com/thma/WhyHaskellMatters/workflows/Haskell%20CI/badge.svg)](https://github.com/thma/WhyHaskellMatters/actions)

> Haskell并没有比其他语言更好的解决不同的问题。
> 但解决方法不同。
> 
> -- 未知

## 摘要

在本文中，我试图通过介绍Haskell的一些最重要和最显著的特性并用工作代码示例详细说明它们，来解释为什么Haskell一直是一种如此重要的语言。

展示的目的是自足，不需要任何语言知识。
目标读者是Haskell的新生和具有非函数语言背景的开发人员，他们渴望学习函数式编程的概念，尤其是Haskell。

## 目录

- [介绍](#介绍)
- [函数是一等公民](#函数是一等公民)
  - [函数可以像任何其他值一样分配给变量](#函数可以像任何其他值一样分配给变量)
  - [支持匿名函数](#支持匿名函数)
  - [函数可以作为其他函数的值返回](#函数可以作为其他函数的值返回)
    - [功能构成](#功能构成)
    - [科里化和部分应用](#科里化和部分应用)
  - [函数可以作为参数传递给其他函数](#函数可以作为参数传递给其他函数)
- [模式匹配](#模式匹配)
- [代数数据类型](#代数数据类型)
- [多态数据类型](#多态数据类型)
  - [列表](#列表)
    - [算术序列](#算术序列)
- [不变性](#不变性)
- [声明式编程](#声明式编程)
  - [映射](#映射)
  - [折叠](#折叠)
- [非严格评估](#非严格评估)
  - [避免无休止的循环](#避免无休止的循环)
  - [定义潜在的无限数据结构](#定义潜在的无限数据结构)
  - [列表理解](#列表理解)
  - [将控制流结构定义为抽象](#将控制流结构定义为抽象)
- [类型类](#类型类)
  - [读取和展示](#读取和展示)
  - [函子与可折叠](#函子与可折叠)
    - [函子](#函子)
    - [可折叠](#可折叠)
  - [The Maybe Monad](#the-maybe-monad)
    -  [Total函数](#total-函数)
    -  [Maybe 操作符的构成](#Maybe 操作符的构成)
  - [存粹性](#存粹性)  
  - [IO-Monad的明显副作用](#IO-Monad的明显副作用)
- [总结](#总结)

## 介绍

整整三十年前，1990年4月1日，一个非严格函数编程领域的小组发表了最初的Haskell语言报告。

Haskell从未成为软件行业中最流行的语言之一或主流语言的一部分，但它在软件开发社区中一直并仍然具有相当大的影响力。

在本文中，我试图通过介绍Haskell的一些最显著的特性并用工作代码示例详细说明它们，来解释为什么Haskell一直是一种如此重要的语言。

展示的目的是自足，不需要任何语言知识。
我也会尽量保持学习曲线的适度，并限制展示的范围；然而，本文绝不是对语言的完整介绍。

> （如果您正在寻找全面的教程，请查看[[Haskell Wikibook]](https://en.wikibooks.org/wiki/Haskell)或[[了解Haskell](http://www.learnyouahaskell.com/)]
>

在直接讨论技术细节之前，我想先详细了解一下Haskell在软件开发社区的表现：

### 随着时间的奇怪发展

在2017年关于[[Haskell之旅](https://www.youtube.com/watch?v=re96UgMk6GQ)]
从20世纪80年代开始西蒙·佩顿·琼斯就谈到相当不寻常的Haskell的故事。

首先，他谈到了研究语言的典型生命周期。它们通常是由一个单独的研究人员（也是唯一的用户）创建的，其中大多数在几年后就会被抛弃。

一种更成功的研究语言可能会在一个更大的社区中获得一些兴趣，但仍然无法逃脱象牙塔的魔爪，而且通常会在十年内被放弃。

另一方面，我们拥有所有那些流行的编程语言，它们很快被大量开发人员采用，从而达到了“不朽的门槛”。这是现有代码的基础，它将变得如此庞大，以至于该语言将使用数十年。

他有点开玩笑地描述了通过零线的委员会：他们永远不会起飞。

最后，他展示了一张显示Haskell时间线的图表：

![the haskell timeline](img/language-5.png)

这张图表显示的发展似乎相当出乎意料：Haskell最初是一种研究语言，甚至是由一个委员会设计的；
所以很可能它早在千年前就应该被抛弃了！

相反，它在早期获得了一些动力，随后在面向对象宣传的十年中进入了一个相当安静的阶段（Java于1995年发布）。
然后我们再次看到，自2005年以来，人们的兴趣不断增长。
我在2020年初写这篇文章，我们仍然看到了这一趋势！

### 被使用与被讨论

然后西蒙·佩顿·琼斯指出了近年来Haskell的另一个有趣的特点：
在按实际使用情况对编程语言进行排序的变化关系中，Haskell通常不低于30种最活跃的语言。
但在统计数据中，语言是按互联网上的讨论量来排列的
Haskell的得分通常要高得多（通常在前十名）。

### 那么为什么Haskell一直是软件开发界的热门话题呢？

一个非常简短的答案可能是：
Haskell有许多明显不同于大多数其他编程语言的特性。
这些特性中的许多已经被证明是解决软件开发基本问题的强大工具。

因此，随着时间的推移，其他编程语言已经采用了这些概念的一部分（例如模式匹配或类型类）。
在讨论这些概念时，提到了Haskell遗产并讨论了原Haskell概念与其他语言的区别。
有时人们会感到鼓舞，因为他们可以更深入地了解这些概念的来源及他们的初衷。这就是为什么我们看到越来越多的开发人员在Python，Typescript，斯卡拉，Rub，C++，C或java开始跳入Haskell。

更重要的一点是，Haskell仍然是一个实验性实验室，在诸如编译程序结构，编程语言设计，定理证明，类型系统等。因此，Haskell将不可避免地成为讨论这些方法的一个话题。

在下面的部分中，我们将通过研究Haskell的一些最显著的特征。

## 函数是一等公民

> 在计算机科学中，一种编程语言如果把函数当作一等公民来对待，就被认为具有一等功能。这意味着语言支持**将函数作为参数传递给其他函数**，
> **将它们作为其他函数**的值返回，并**将它们分配给变量或存储在数据结构中。**[1]一些编程语言理论家要求**支持匿名函数**（函数文本）
> 在具有一级函数的语言中，函数的名称没有任何特殊的状态；它们被视为具有函数类型的普通变量。
> 
> 注释来自于 [Wikipedia](https://en.wikipedia.org/wiki/First-class_function)

我们将一点一点进行分析:

### 函数可以像任何其他值一样分配给变量

让我们看看Haskell的情况。首先，我们定义一些简单的值：

```haskell
-- define constant `aNumber` with a value of 42. 
aNumber :: Integer
aNumber = 42

-- define constant `aString` with a value of "hello world"
aString :: String
aString = "Hello World"
```

在第一行中，我们看到一个类型签名，它将常量“aNumber”定义为“Integer”类型。
在第二行中，我们将“aNumber”的值定义为“42”。
同样，我们将常数aString定义为String类型。

Haskell是一种静态类型语言：所有类型检查都在编译时进行。
静态类型的优点是类型错误不会在运行时发生。
如果更改了函数签名，并且此更改影响项目的许多依赖部分：编译器将检测中断的更改在所有受影响的地方。

Haskell编译器还提供*类型推断*，允许编译器推断具体的数据类型来自上下文的表达式。
因此，通常不需要提供类型声明。然而，使用显式类型签名被认为是一种好的样式，因为它们是全面的文件。

接下来，我们定义一个函数“square”，它接受一个整型参数并返回该参数的平方值：

```Haskell
square :: Integer -> Integer
square x = x * x
```

函数定义的工作方式与任何其他值的定义完全相同。唯一特别的是，我们使用“->”符号将类型声明为**函数类型**。
所以“：：Integer->Integer”表示从“Integer”到“Integer”的函数。在第二行中，我们定义了“square”函数来计算任何“Integer”参数“x”的“x*x”。

好吧，似乎不太难，那么让我们定义另一个函数double，它的输入值是double的两倍：

```haskell
double :: Integer -> Integer
double n = 2 * n
```

### 支持匿名函数

匿名函数，也称为lambda表达式，可以在Haskell中定义如下：

```Haskell
\x -> x * x
```

此表达式表示一个匿名函数，该函数接受单个参数x并返回该参数的平方。反斜杠读作λ（希腊字母lambda）。

您可以在任何使用其他函数的地方使用这样的表达式。例如，您可以应用匿名函数`\x->x*x`的值与命名函数`square`的值相同：

```haskell
-- use named function:
result = square 5

-- use anonymous function:
result' = (\x -> x * x) 5
```

我们将在下一节中看到匿名函数的更有用的应用程序。

### 函数可以作为其他函数的值返回

#### 函数构成

你还记得高中数学课上的函数作文吗？
函数合成是一种操作，它接受两个函数“f”和“g”，并生成一个函数“h”，这样
`h(x) = g(f(x))`
得到的复合函数表示为“h=g ∘ f ` where`（g ∘ f ）（x）=g（f（x））`。
直观地说，组合函数是一个链式过程，其中函数“f”的输出用作函数“g”的输入。

所以从程序员的角度来看，运算符是一个
接受两个函数作为参数并返回一个新的复合函数。

在Haskell中，此运算符表示为点运算符`.`:

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g x = f (g x)
```

点周围的括号是必需的，因为我们希望使用非字母符号作为标识符。在Haskell中，这样的标识符可以用作中缀运算符（如下所示）。否则，“（.）”被定义为任何其他函数。请注意语法与原始数学定义的接近程度。

点周围的括号是必需的，因为我们希望使用非字母符号作为标识符。在Haskell中，这样的标识符可以用作中缀运算符（如下所示）。否则，“（.）”被定义为任何其他函数。请注意语法与原始数学定义的接近程度。

使用这个运算符，我们可以轻松地创建一个复合函数。该函数首先将一个数加倍，然后计算该加倍数的平方：

```haskell
squareAfterDouble :: Integer -> Integer
squareAfterDouble = square . double
```

#### 科里化和部分应用

在本节中，我们将看到另一个有趣的函数生成示例其他函数作为返回值。

我们首先定义一个函数“add”，它接受两个“Integer”参数并计算它们的和：

```haskell
-- function adding two numbers 
add :: Integer -> Integer -> Integer
add x y = x + y
```

这看起来很直截了当。但仍有一个有趣的细节值得注意：
“add”的类型签名不是

```haskell
add :: (Integer, Integer) -> Integer
```

取而代之的是：

```haskell
add :: Integer -> Integer -> Integer
```

这个签名到底是什么意思？
它可以读作“一个函数，它接受一个整型参数并返回一个类型为“Integer->Integer”的函数”。
听起来很奇怪？但这正是哈斯克尔在内部所做的。
因此，如果我们调用add 2 3，那么首先将add应用于'2'，该函数返回一个新的Integer->Integer类型的函数，然后将其应用于'3'。

这种技术叫做 [**Currying**](https://wiki.haskell.org/Currying)

curring在Haskell中被广泛使用，因为它允许另一个很酷的东西：**部分程序**。

在下一个代码片段中，我们定义了一个函数“add5”，方法是将函数“add”部分应用于一个参数：

```haskell
-- partial application: applying add to 5 returns a function of type Integer -> Integer
add5 :: Integer -> Integer
add5 = add 5
```

诀窍如下：“add 5”返回一个“Integer”->“Integer”类型的函数，该函数将向任何整数参数添加“5”。

因此，部分应用程序允许我们编写将函数作为结果值返回的函数。
这种技术经常被用来
[为功能提供配置数据](https://github.com/thma/LtuPatternFactory#dependency-injection--parameter-binding-partial-application).

### 函数可以作为参数传递给其他函数

我可以简短地告诉您，我们已经看到了这样一个例子：
函数组合运算符`（.）`。
它**接受两个函数作为参数**，并返回一个新函数，如下所示：

```haskell
squareAfterDouble :: Integer -> Integer
squareAfterDouble = square . double
```

但我手头还有一个很有启发性的例子。

假设我们必须实现一个函数，使任何奇数的整数加倍：

```haskell
ifOddDouble :: Integer -> Integer
ifOddDouble n =
  if odd n
    then double n
    else n
```

Haskell代码很简单：新的成分是if...然后...否则…`and the odd`odd`是Haskell标准库中的谓词
如果整数是奇数，则返回“True”。
现在假设我们还需要另一个函数来计算任意奇数的平方：

```haskell
ifOddSquare :: Integer -> Integer
ifOddSquare n =
  if odd n
    then square n
    else n
```

作为警惕的开发人员，我们立即发现[不要重复自己的原则](https://en.wikipedia.org/wiki/Don%27t_repeat_yourself) 因为这两个函数只在使用不同的增长函数“double”和“square”时有所不同。

因此，我们正在寻找一种方法，通过保留原始结构但允许改变使用的增长函数的解决方案来重构此代码。

我们需要的是一个函数，它将一个增长函数（类型为`（Integer->Integer）`）作为第一个参数，一个‘Integer’作为第二个参数，并返回一个‘Integer’。指定的增长函数将应用于“then”子句：

```haskell
ifOdd :: (Integer -> Integer) -> Integer -> Integer
ifOdd growthFunction n =
  if odd n
    then growthFunction n
    else n
```

使用这种方法，我们可以重构“ifodddoule”和“ifOddSquare”，如下所示：第二个参数并返回“Integer”。指定的增长函数将应用于“then”子句：

```haskell
ifOddDouble :: Integer -> Integer
ifOddDouble n = ifOdd double n

ifOddSquare :: Integer -> Integer
ifOddSquare n = ifOdd square n
```

现在假设我们必须实现新函数“ifevendoule”和“ifEvenSquare”只对偶数有效。我们没有重复自己，而是想出了一个函数ifPredGrow`以`（Integer->Bool）类型的谓词函数作为第一个参数，类型为`（Integer->Integer）`的增长函数作为第二个参数，整数作为第三个参数，返回一个“整数”。

谓词函数将用于确定是否必须应用增长函数：

```haskell
ifPredGrow :: (Integer -> Bool) -> (Integer -> Integer) -> Integer -> Integer
ifPredGrow predicate growthFunction n =
  if predicate n
    then growthFunction n
    else n
```

使用 [高阶函数](https://en.wikipedia.org/wiki/Higher-order_function) 我们甚至可以将两个函数作为参数来编写这两个新函数，并进一步重构现有的函数，而不破坏DRY原则：

```haskell
ifEvenDouble :: Integer -> Integer
ifEvenDouble n = ifPredGrow even double n

ifEvenSquare :: Integer -> Integer
ifEvenSquare n = ifPredGrow even square n

ifOddDouble'' :: Integer -> Integer
ifOddDouble'' n = ifPredGrow odd double n

ifOddSquare'' :: Integer -> Integer
ifOddSquare'' n = ifPredGrow odd square n
```

## 模式匹配

根据我们目前所学的知识，我们现在可以开始实现一些更有趣的功能。
那么如何实现递归 [阶乘函数](https://en.wikipedia.org/wiki/Factorial)?

阶乘函数的定义如下：

> For all n ∈ ℕ<sub>0</sub>:
>```
>0! = 1
>n! = n * (n-1)!
>```

根据我们目前对Haskell的了解，我们可以实现如下功能：

```haskell
factorial :: Natural -> Natural
factorial n =
  if n == 0
    then 1
    else n * factorial (n - 1)
```

我们使用Haskell数据类型“Natural”来表示非负整数集。
在函数“factorial”的定义中使用文本“factorial”按预期工作，并表示递归函数调用。

由于函数的这种递归定义是函数编程的典型，语言设计者有添加了一个名为“模式匹配”的有用功能，允许通过一组方程式定义函数：

由于函数的这种递归定义是函数编程的典型，语言设计者有添加了一个名为“模式匹配”的有用功能，允许通过一组方程式定义函数：

```haskell
fac :: Natural -> Natural
fac 0 = 1
fac n = n * fac (n - 1)
```

这种样式更接近于数学定义，并且通常更可读，因为它有助于避免嵌套 `if ... then ... else ...`的构造.

模式匹配不仅可以用于数值，还可以用于任何其他数据类型。我们很快会看到更多的例子。

## 代数数据类型

Haskell通过使用一个经过深思熟虑的概念来支持用户定义的数据类型。
让我们从一个简单的例子开始：

```haskell
data Status = Green | Yellow | Red
```

这声明了一个数据类型'Status'，它正好有三个不同的实例。每种情况下定义了数据构造函数*以允许创建数据类型的新实例。

这些数据构造函数中的每一个都是返回“Status”实例的函数（在本例中是常量）。

类型“Status”是一个所谓的*sum type*，因为它表示三者之和定义的集合实例“Green”、“Yellow”、“Red”。在Java中，这对应于枚举。

假设我们必须创建一个将“Status”值映射到“Severity”值的转换器表示其他系统中的严重性级别。
这个转换器可以使用我们在上面已经看到的模式匹配语法来编写：

```haskell
-- another sum type representing severity:
data Severity = Low | Middle | High deriving (Eq, Show)

severity :: Status -> Severity
severity Green  = Low
severity Yellow = Middle
severity Red    = High
```

编译器将告诉我们何时没有覆盖“Status”类型的所有实例（通过使用`-fwarn incomplete patterns`注释）。

现在我们来看组合多个不同元素的数据类型，如成对n元组等。让我们从结合两个不同元素的“PairStatusSeverity”类型开始：

```haskell
data PairStatusSeverity = P Status Severity
```

这可以理解为：数据类型“PairStatusSeverity”可以由数据构造函数“P”，它接受“Status”类型的值和“Severity”类型的值，并返回“Pair”实例。

例如'P Green High'返回'PairStatusSeverity'实例（数据构造函数“P”具有签名“P：：Status->Severity->PairStatusSeverity”）。

类型“PairStatusSeverity”可以解释为所有可能的状态和严重性值的有序对的集合，这是“Status”和“Severity”的笛卡尔积。

这就是为什么这种数据类型被称为*产品类型*。

Haskell允许您通过组合*sum types*和*product types*来创建任意数据类型。完整的可以用这种方式构造的数据类型的范围称为
[*代数数据类型*](https://en.wikipedia.org/wiki/Algebraic_data_type) 或 ADT.

- 使用代数数据类型有几个优点：
  
  - ​    模式匹配可用于分析任何具体实例，以根据输入数据选择不同的行为。
  
    ​		在将“Status”映射到“Severity”的示例中，不需要使用“if..then..else..”构造。
  
  - 编译器可以检测不完整的模式匹配或其他缺陷。
  
  - 编译器可以为adt自动派生许多复杂的功能，因为它们是在这样有规律。
  
  我们将在下面的章节中介绍adt和模式匹配的有趣组合。

## 多态数据类型

在编程中，形成成对或更普遍的n元组是一项非常常见的任务。
因此，如果我们被迫创建新的Pair或Tuple类型，这将是不方便和重复的每种具体用途。请考虑以下示例：

```haskell
data PairStatusSeverity = P Status Severity

data PairStatusString   = P' Status String

data PairSeverityStatus = P'' Severity Status
```

幸运的是，数据类型声明允许使用类型变量来避免这种混乱的代码。
因此，我们可以定义一个通用数据类型“Pair”，它允许我们自由组合不同类型的参数：

```haskell
-- a simple polymorphic type
data Pair a b = P a b
```

这可以理解为：数据类型“Pair”使用两个（可能）不同类型“a”和“b”的元素数据构造函数“P”接受“a”类型的值和“b”类型的值，并返回“Pair a b”实例（数据构造函数“P”具有签名“P：：a->b->Pair a b”）。
类型“Pair”现在可以用来创建许多不同的具体数据类型称为*多态*数据类型。
由于多态性是由类型变量（即类型声明的参数）定义的，因此这种机制是称为*参数多态性*。

由于成对和n元组的使用非常频繁，Haskell语言设计人员在和他们一起轻松地工作。

所以您可以简单地编写如下元组：

```haskell
tuple :: (Status, Severity, String)
tuple = (Green, Low, "All green")
```

### 列表

另一个非常有用的多态类型是“List”。

列表可以是空列表（由数据构造函数`[]`表示）
或者是数据类型“a”的某个元素，后跟一个包含“a”类型元素的列表，用“[a]”表示。

这种直觉反映在以下数据类型定义中：

```haskell
data [a] = [] | a : [a]
```

cons运算符`（：）`（上一节中类似于`（.）`的中缀运算符）声明为数据构造函数*从类型为“a”的单个元素和类型为“[a]”的列表构造列表。

因此，仅包含单个元素“1”的列表由以下元素构成：

```haskell
1 : []
```

包含三个数字1、2、3的列表是这样构造的：

```haskell
1 : 2 : 3 : []
```

幸运的是，Haskell语言设计人员非常友好地为此提供了一些语法糖。
因此，第一个列表可以简单地写成`[1]'，第二个列表可以写成`[1,2,3]`。

多态类型表达式描述*类型的系列*。
例如，`（forall a）[a]`是由以下类型组成的类型系列：，
对于每种类型“a”，都是“a”列表的类型。
整数列表（例如，`[1,2,3]`），字符列表（`['a'，'b'，'c']`），
甚至整数列表等都是这个家族的成员。

处理列表的函数可以使用模式匹配为`[]`和`a:[a]`案例选择行为。

以计算列表长度的函数“length”的定义为例：

```haskell
length :: [a] -> Integer
length []     =  0
length (x:xs) =  1 + length xs
```

我们可以把这些方程理解为：空列表的长度是0，第一个元素是x，其余元素是xs的列表的长度是1加上xs的长度。


在下一个例子中，我们要处理一些随机整数中的a：

```haskell
someNumbers :: [Integer]
someNumbers = [49,64,97,54,19,90,934,22,215,6,68,325,720,8082,1,33,31]
```

现在我们要从这个列表中选择所有偶数或所有奇数。我们正在寻找一个需要两个参数：首先是一个谓词函数，用于检查每个元素其次是元素的实际列表。函数将返回一个包含所有匹配元素的列表。当然，我们的解决方案不仅适用于整数，也适用于任何其他类型。
下面是此类筛选函数的类型签名：

```haskell
filter :: (a -> Bool) -> [a] -> [a]
```

在实现中，我们将使用模式匹配为`[]`和`（x:xs）`情况提供不同的行为：

```haskell
filter :: (a -> Bool) -> [a] -> [a]
filter pred []     = []
filter pred (x:xs)
  | pred x         = x : filter pred xs
  | otherwise      = filter pred xs
```

很明显是`[]`案件。为了理解`（x:xs）`情况，我们必须知道除了类型构造函数的简单匹配之外
我们还可以使用*模式保护*对输入数据执行附加测试。在本例中，如果pred x的计算结果为True，则x是匹配项，将与过滤pred xs`。
如果计算结果不是'True'，我们不会将“x”添加到结果列表中，因此只需对列表的其余部分递归调用filter。

现在，我们可以使用“filter”从示例列表中选择元素：

```haskell
someEvenNumbers :: [Integer]
someEvenNumbers = filter even someNumbers

-- predicates may also be lambda-expresssions
someOddNumbers :: [Integer]
someOddNumbers = filter (\n -> n `rem` 2 /= 0) someNumbers  
```

当然，我们不必自己发明像“filter”这样的函数，但可以在Haskell基础类库中依赖 [处理列表的大量预定义函数集](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-List.html) 


#### 算术序列

在处理数字列表时，有一个很好的特性经常派上用场。它叫做算术序列允许您使用简洁的语法定义数字列表：

```haskell
upToHundred :: [Integer]
upToHundred = [1..100]
```

正如预期的那样，这将使用1到100之间的整数列表指定“upto100”。

也可以定义一个步长来确定后续数字之间的增量。
如果我们只需要奇数，我们可以这样构造它们：

```haskell
oddsUpToHundred :: [Integer]
oddsUpToHundred = [1,3..100]
```

算术序列也可以用于更动态的情况。例如，我们可以这样定义“factorial”函数：
```math
n! = 1 * 2 * 3 ... (n-2) * (n-1) * n, for integers > 0
```

在Haskell中，我们可以使用算术序列定义此函数：

```haskell
fac' n   = prod [1..n]
```

## 不可变性

> 在面向对象和函数式编程中，不可变对象是一个在创建后其状态不能修改的对象。这与可变对象（可变对象）形成对比，可变对象可以在创建后进行修改。
> 
> 引自 [Wikipedia](https://en.wikipedia.org/wiki/Immutable_object)

这将是一个非常短的部分。在Haskell中，所有数据都是不可变的。周期。

让我们来看看与Haskell GHCi REPL的一些交互（每当您在本文中看到“λ>”提示时
它来自GHCi会议）：

```haskell
λ> a = [1,2,3]
λ> a
[1,2,3]
λ> reverse a
[3,2,1]
λ> a
[1,2,3]
```

在Haskell中，无法在“a”的初始创建之后更改其值。没有破坏性的*在其他一些函数式语言（如Lisp、Scheme或ML）中可用的操作。

这样做的巨大好处是重构变得比语言中的每个函数或方法都要简单得多可能会改变数据。因此，对一段给定的代码进行推理也会更加容易。

当然，这也使得并发操作的编程更加容易。用“什么都不分享”的方法，Haskell程序将自动是线程安全的。

## 声明式编程

在本节中，我想解释如何使用*高阶*函数编程从用户代码中找出许多基本的控制结构和算法。

这将产生一种更具*声明性的编程*风格，开发人员可以简单地说明她想要实现什么，但不需要写下如何实现。

应用这种风格的代码将更加密集，它将更加关注实际的元素问题域的比与技术实现细节。

### 映射

我们将通过一些处理列表的示例来演示这一点。首先，我们要编写一个函数，将一个`[Integer]`列表的所有元素加倍。我们希望重用上面已经定义的“double”函数。

到目前为止，我们所学到的编写函数doubleAll并不难：

```haskell
-- compute the double value for all list elements
doubleAll :: [Integer] -> [Integer]
doubleAll [] = []
doubleAll (n:rest) = double n : doubleAll rest
```

接下来我们要实现一个类似的函数'square all'，它将使用'square'计算列表中所有元素的平方。
最简单的方法是在*WET*（我们喜欢打字）方法中实现它：

```haskell
-- compute squares for all list elements
squareAll :: [Integer] -> [Integer]
squareAll [] = []
squareAll (n:rest) = square n : squareAll rest
```

当然这很难看：
两个函数使用相同的模式匹配并应用相同的递归迭代策略。它们只在应用于每个元素的函数上有所不同。

作为榜样开发者，我们不想重蹈覆辙。因此，我们正在寻找捕获在元素列表上映射给定函数的本质：

```haskell
map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs
```

此函数抽象出遍历列表的实现细节，并允许提供用户定义的映射函数也是。

现在我们可以使用“map”简单地*声明我们的意图*（“what”），而不必详细说明“how”：

```haskell
doubleAll' :: [Integer] -> [Integer]
doubleAll' = map double

squareAll' :: [Integer] -> [Integer]
squareAll' = map square
```

### 折叠

现在让我们来看看一些相关的问题。
我们的第一个任务是将一个`[Integer]`列表的所有元素相加。
首先是使用已经熟悉的模式匹配和递归混合的朴素方法：

```haskell
sumUp :: [Integer] -> Integer
sumUp [] = 0
sumUp (n:rest) = n + sumUp rest
```

通过查看计算`[Integer]`列表中所有元素乘积的函数的代码，我们可以再次看到我们一直在重复：

```haskell
prod :: [Integer] -> Integer
prod [] = 1
prod (n:rest) = n * prod rest
```

那么这两种算法的本质是什么？
在这两种算法的核心，我们有一个递归函数

- 接受一个二进制运算符（在本例中为“+”或“（*）”），
- 用作累积起点的初始值
  （通常是二进制运算符的标识元素（或中性元素），
- 应减少为单个返回值的元素列表
- 通过递归地将二进制运算符应用于列表的所有元素，直到到达`[]'，来执行累加，
  返回中性元素的位置。

这个本质包含在更高阶函数“foldr”中，它也是Haskell标准库的一部分：

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc []     =  acc
foldr f acc (x:xs) =  f x (foldr f acc xs)
```

现在我们可以使用“foldr”简单地*声明我们的意图*（“what”），而不必详细说明“how”：

```haskell
sumUp' :: [Integer] -> Integer
sumUp' = foldr (+) 0

prod' :: [Integer] -> Integer
prod' = foldr (*) 1
```

有了函数“map”和“foldr”（或“reduce”），我们现在有了两个非常强大的工具，可以在许多需要处理列表数据的情况下使用。

这两个函数甚至可以组合成另一个非常重要的编程概念：*Map/Reduce*。
在Haskell中，此操作由函数'foldMap'提供。

我不会在这里详细讨论，因为它超出了本文的范围。但我会邀请你读我的
[介绍Haskell中的 Map/Reduce ](https://github.com/thma/LtuPatternFactory#map-reduce).

## 非严格评估

现在我们来讨论一下Haskell设计师的主要驱动因素之一：他们希望远离了当时标准的严格评价模式。

非严格评估（aka。正规降阶）有一个非常重要的性质。

> 如果lambda表达式具有范式，则范式降阶将终止并找到该范式。
>
> Church-Rosser Theorem II

对于其他缩减策略（如应用程序顺序或按值缩减调用），此属性不适用。

这是数学研究的结果 [lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus) 很重要，因为Haskell维护了正规降阶的语义。

懒惰评估的实际好处包括：

- 在某些边缘情况下避免无休止的循环
- 将控制流（结构）定义为抽象而不是原语的能力。
- 定义潜在无限数据结构的能力。这允许更直接地实现一些算法。

因此，让我们更深入地了解这些好处：

### 避免无终止循环

考虑以下示例函数：

```haskell
ignoreY :: Integer -> Integer -> Integer
ignoreY x y = x
```

它接受两个整型参数并返回第一个未修改的参数。第二个论点被忽略了。

在大多数编程语言中，两个参数都是在执行函数体之前求值：
他们使用应用的订单缩减aka。急切求值或按值调用语义。

另一方面，在Haskell中，使用第二个参数中的非终止表达式调用函数是保存的。
首先，我们创建一个非终止表达式“恶性循环”。任何对其进行评估的尝试都将导致无休止的循环：

```haskell
-- it's possible to define non-terminating expressions like
viciousCircle :: a
viciousCircle = viciousCircle
```

但是如果我们使用“viciousCircle”作为函数“ignoreY”的第二个参数，它将被忽略，第一个参数返回


```haskell
-- trying it in GHCi:
λ> ignoreY 42 viciousCircle
42
```

### 定义潜在的无限数据结构

在 [列表的部分中](#列表) 我们已经见过算术序列 `[1..10]`.

算术序列也可以用来定义无限的数字列表。
下面是几个例子：

```haskell
-- all natural numbers
naturalNumbers = [1..]

-- all even numbers
evens = [2,4..]

-- all odd numbers
odds  = [1,3..]
```

定义这些无限列表是相当容易的。但我们能对付他们吗？它们有用吗？在上面的“恶性循环”示例中，我们了解到定义该表达式是很好的，但是任何对其求值的尝试都将导致无限循环。

如果我们试着打印“自然数”，我们也会在打印到屏幕上的整数的无限循环中结束。

但如果我们比要求所有自然数少一点贪婪，一切都会好的。

```haskell
λ> take 10 naturalNumbers
[1,2,3,4,5,6,7,8,9,10]

λ> take 10 evens
[2,4,6,8,10,12,14,16,18,20]

λ> take 10 odds
[1,3,5,7,9,11,13,15,17,19]
```

我们还可以在这样一个无限列表中的特定位置使用`（！！）`操作符：

```haskell
λ> odds !! 5000
10001

λ> evens !! 10000
20002
```

### List comprehension

Do you remember *set comprehension* notation from your math classes?

As simple example would be the definition of the set of even numbers:

> Evens = {i | i = 2n ∧ n ∊ ℕ}

Which can be read as: Evens is defined as the set of all `i` where `i = 2*n` and `n` is an element of the set of natural numbers.

The Haskell *list comprehension* allows us to define - potentially infinite - lists with a similar syntax:

```haskell
evens' = [2*n | n <- [1..]]
```

Again we can avoid infinite loops by evaluating only a finite subset of `evens'`:

```haskell
λ> take 10 evens'
[2,4,6,8,10,12,14,16,18,20]
```

List comprehension can be very useful for defining numerical sets and series in a (mostly) declarative way that comes 
close to the original mathematical definitions.

Take for example the set `PT` of all pythagorean triples

>  PT = {(a,b,c) | a,b,c ∊ ℕ ∧ a² + b² = c² }

The Haskell definition looks like this:

```haskell
pt :: [(Natural,Natural,Natural)]
pt = [(a,b,c) | c <- [1..],
                b <- [1..c],
                a <- [1..b],
                a^2 + b^2 == c^2]
```

### Define control flow structures as abstractions

In most languages it is not possible to define new conditional operations, e.g. your own `myIf` statement.
A conditional operation will evaluate some of its arguments only if certain conditions are met.
This is very hard - if not impossible - to implement in language with call-by-value semantics which evaluates all function arguments before
actually evaluating the function body.

As Haskell implements call-by-need semantics, it is possible to define new conditional operations.
In fact this is quite helpful when writing *domain specific languages*.

Here comes a very simple version of `myIf`:

```haskell
myIf :: Bool -> b -> b -> b
myIf p x y = if p then x else y 

λ> myIf (4 > 2) "true" viciousCircle
"true"
```

A somewhat more useful control-structure is the `cond` (for conditional) function that stems from LISP and Scheme languages.
It allows you to define a more table-like decision structure, somewhat resembling a `switch` statement from C-style languages:

```haskell
cond :: [(Bool, a)] -> a
cond []                 = error "make sure that at least one condition is true"
cond ((True,  v):rest)  = v
cond ((False, _):rest)  = cond rest
```

With this function we can implement a signum function `sign` as follows:

```haskell
sign :: (Ord a, Num a) => a -> a
sign x = cond [(x > 0     , 1 )
              ,(x < 0     , -1)
              ,(otherwise , 0 )]

λ> sign 5
1
λ> sign 0
0
λ> sign (-4)
-1
```

## Type Classes

Now we come to one of the most distinguishing features of Haskell: *type classes*.

In the section [Polymorphic Data Types](#polymorphic-data-types) we have seen that type variables (or parameters) allow 
type declarations to be polymorphic like in:

```haskell
data [a] = [] | a : [a]
```

This approach is called *parametric polymorphism* and is used in several programming languages.

Type classes on the other hand address *ad hoc polymorphism* of data types. This approach is also known as
*overloading*.

To get a first intuition let's start with a simple example.

We would like to be able to use characters (represented by the data type `Char`) as if they were numbers.
E.g. we would like to be able to things like:

```haskell
λ> 'A' + 25
'Z'

-- please note that in Haskell a string is List of characters: type String = [Char]
λ> map (+ 5) "hello world"
"mjqqt%|twqi"

λ> map (\c -> c - 5) "mjqqt%|twqi"
"hello world"
```

To enable this we will have to *overload* the infix operators `(+)` and `(-)` to work not only on numbers but also on characters.
Now, let's have a look at the type signature of the `(+)` operator:

```haskell
λ> :type (+)
(+) :: Num a => a -> a -> a
```

So `(+)` is not just declared to be of type `(+) :: a -> a -> a` but it contains a **constraint** on the type variable `a`, 
namely `Num a =>`. 
The whole type signature of `(+)` can be read as: for all types `a` that are members of the type class `Num` the operator `(+)` has the type
`a -> a -> a`.

Next we obtain more information on the type class `Num`:

```haskell
λ> :info Num
class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
  {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
  	-- Defined in `GHC.Num'
instance Num Word -- Defined in `GHC.Num'
instance Num Integer -- Defined in `GHC.Num'
instance Num Int -- Defined in `GHC.Num'
instance Num Float -- Defined in `GHC.Float'
instance Num Double -- Defined in `GHC.Float'
```

This information details what functions a type `a` has to implement to be used as an instance of the `Num` type class.
The line `{-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}` tells us what a minimal complete implementation
has to provide.
It also tells us that the types `Word`, `Integer`, `Int`, `Float` and `Double` are instances of the `Num` type class.

This is all we need to know to make the type `Char` an instance of the `Num` type class, so without further ado we
dive into the implementation (please note that `fromEnum` converts a `Char` into an `Int` and `toEnum` converts 
an `Int` into an `Char`):

```haskell
instance Num Char where
  a + b       = toEnum (fromEnum a + fromEnum b)
  a - b       = toEnum (fromEnum a - fromEnum b)
  a * b       = toEnum (fromEnum a * fromEnum b)
  abs c       = c
  signum      = toEnum . signum . fromEnum
  fromInteger = toEnum . fromInteger
  negate c    = c
```

This piece of code makes the type `Char` an instance of the `Num` type class. We can then use `(+)` and `(-) as demonstrated
above.

Originally the idea for type classes came up to provide overloading of arithmetic operators
in order to use the same operators across all numeric types.

But the type classes concept proved to be useful in a variety of other cases as well. 
This has lead to a rich sets of type classes provided by the Haskell base library and
a wealth of programming techniques that make use of this powerful concept.

Here comes a graphic overview of some of the most important type classes in the Haskell base library:

![The hierarchy of basic type classes](https://upload.wikimedia.org/wikipedia/commons/thumb/0/04/Base-classes.svg/510px-Base-classes.svg.png)

I won't go all of these but I I'll cover some of the most important ones.

Let's start with Eq:

```haskell
class  Eq a  where
   (==), (/=) :: a -> a -> Bool

       -- Minimal complete definition:
       --      (==) or (/=)
   x /= y     =  not (x == y)
   x == y     =  not (x /= y)
```

This definition states two things: 

- if a type `a` is to be made an instance of the class `Eq` it must support the 
  functions `(==)` and `(/=)` both of them having  type `a -> a -> Bool`.  
- `Eq` provides *default definitions* for `(==)` and `(/=)` in terms of each other. 
   As a consequence, there is no need for a type in `Eq` to provide both definitions - 
   given one of them, the other will work automatically.

Now we can turn some of the data types that we defined in the section on 
[Algebraic Data Types](#algebraic-data-types) into instances of the `Eq` type class.

Here the type declarations as a recap:

```haskell
data Status   = Green | Yellow | Red
data Severity = Low | Middle | High 
data PairStatusSeverity = PSS Status Severity
```

First, we create Eq instances for the simple types `Status` and `Severity` by defining the `(==)` 
operator for each of them:

```haskell
instance Eq Status where
  Green  == Green  = True
  Yellow == Yellow = True
  Red    == Red    = True
  _      == _      = False
  
instance Eq Severity where
  Low    == Low    = True
  Middle == Middle = True
  High   == High   = True
  _      == _      = False
```

Next, we create an `Eq` instance for `PairStatusSeverity` by defining the `(==)` operator:

```haskell
instance Eq PairStatusSeverity where
   (PSS sta1 sev1) == (PSS sta2 sev2) = (sta1 == sta2) && (sev1 == sev2)
```

With these definitions it is now possible to use the `(==)` and `(/=)` on our three types.

As you will have noticed, the code for implementing `Eq` is quite boring. Even a machine could do it!

That's why the language designers have provided a `deriving` mechanism to let the compiler automatically implement
type class instances if it's automatically derivable as in the `Eq` case.

With this syntax it much easier to let a type implement the `Eq` type class:

```haskell
data Status   = Green | Yellow | Red          deriving (Eq)
data Severity = Low | Middle | High           deriving (Eq)
data PairStatusSeverity = PSS Status Severity deriving (Eq)
```

This automatic deriving of type class instances works for many cases and reduces a lof of repetitive code.

For example, its possible to automatically derive instances of the `Ord` type class, which provides
ordering functionality:

```haskell
class  (Eq a) => Ord a  where
    compare              :: a -> a -> Ordering
    (<), (<=), (>), (>=) :: a -> a -> Bool
    max, min             :: a -> a -> a
    ...
```

If you are using `deriving` for the `Status` and `Severity` types, the Compiler will implement the
ordering according to the ordering of the constructors in the type declaration.
That is `Green < Yellow < Red` and `Low < Middle < High`:

```haskell
data Status   = Green | Yellow | Red          deriving (Eq, Ord)
data Severity = Low | Middle | High           deriving (Eq, Ord)
```

### Read and Show

Two other quite useful type classes are `Read` and `Show` that also support automatic deriving. 

`Show` provides a function `show` with the following type signature:

```haskell
show :: Show a => a -> String
```

This means that any type implementing `Show` can be converted (or *marshalled*) into a `String` representation.
Creation of a `Show` instance can be achieved by adding a `deriving (Show)` clause to the type declaration.

```haskell
data PairStatusSeverity = PSS Status Severity deriving (Show)

λ> show (PSS Green Low)
"PSS Green Low"
```

The `Read` type class is used to do the opposite: *unmarshalling* data from a String with the function `read`:

```haskell
read :: Read a => String -> a
```

This signature says that for any type `a` implementing the `Read` type class the function `read` can
reconstruct an instance of `a` from its String representation:

```haskell
data PairStatusSeverity = PSS Status Severity deriving (Show, Read)
data Status = Green | Yellow | Red            deriving (Show, Read)
data Severity = Low | Middle | High           deriving (Show, Read)

λ> marshalled = show (PSS Green Low)

λ> read marshalled :: PairStatusSeverity
PSS Green Low
```

Please note that it is required to specify the expected target type with the `:: PairStatusSeverity` clause.
Haskell uses static compile time typing. At compile time there is no way to determine which type
an expression `read "some string content"` will return. Thus expected type must be specified at compile time.
Either by an implicit declaration given by some function type signature, or as in the example above,
by an explicit declaration.

Together `show` and `read` provide a convenient way to serialize (marshal) and deserialize (unmarshal) Haskell
data structures.
This mechanism does not provide any optimized binary representation, but it is still good enough for
many practical purposes, the format is more compact than JSON, and it does not require a parser library.

### Functor and Foldable

The most interesting type classes are those derived from abstract algebra or category theory.
Studying them is a very rewarding process that I'm highly recommending. However, it is definitely
beyond the scope of the present article. Thus, I'm only pointing to two resources covering this part of the Haskell
type class hierarchy.
The first one is the legendary [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia) by Brent Yorgey. 
The second one is [Lambda the ultimate Pattern Factory](https://github.com/thma/LtuPatternFactory)  by myself. 
This text is relating the algebraic type classes to software design patterns.

So we will only cover some of these type classes.

In the section on [declarative programming](#declarative-programming) we came across to very useful concepts:

- mapping a function over all elements of a list (`map :: (a -> b) -> [a] -> [b]`)
- reducing a list with a binary operation and the neutral (identity) element of that operation 
  (`foldr :: (a -> b -> b) -> b -> [a] -> b`)

These concepts are not only useful for lists, but also for many other data structures. So it doesn't come as a
surprise that there type classes that abstract these concepts.

#### Functor

The `Functor` type class generalizes the functionality of applying a function to a value in a context without altering the context, 
(e.g. mapping a function over a list `[a]` which returns a new list `[b]` of the same length):

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

Let's a closer look at this idea by playing with a simple binary tree:

```haskell
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)

-- a simple instance binary tree:
statusTree :: Tree Status
statusTree = Node (Leaf Green) (Node (Leaf Red) (Leaf Yellow))

-- a function mapping Status to Severity
toSeverity :: Status -> Severity
toSeverity Green  = Low
toSeverity Yellow = Middle
toSeverity Red    = High
```

We want to use the function `toSeverity :: Status -> Severity` to convert all `Status` elements of the `statusTree`
into `Severity` instances.

Therefore, we let `Tree` instantiate the `Functor` class:

```haskell
instance Functor Tree where
  fmap f (Leaf a)   = Leaf (f a)
  fmap f (Node a b) = Node (fmap f a) (fmap f b)
```


We can now use `fmap` on `Tree` data structures:

```haskell
λ> fmap toSeverity statusTree
Node (Leaf Low) (Node (Leaf High) (Leaf Middle))
λ> :type it
it :: Tree Severity
```

As already described above, fmap maintains the tree structure unchanged but converts the type of each `Leaf` element, 
which effectively changes the type of the tree to `Tree Severity`.

As derivation of `Functor` instances is a boring task, it is again possible to use the `deriving` clause to
let data types instantiate `Functor`:

```haskell
{-# LANGUAGE DeriveFunctor #-} -- this pragma allows automatic deriving of Functor instances
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, Functor)
```

#### Foldable

As already mentioned, `Foldable` provides the ability to perform *folding* operations on any data type instantiating the
`Foldable` type class:

```haskell
class Foldable t where
  fold    :: Monoid m => t m -> m
  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldr   :: (a -> b -> b) -> b -> t a -> b
  foldr'  :: (a -> b -> b) -> b -> t a -> b
  foldl   :: (b -> a -> b) -> b -> t a -> b
  foldl'  :: (b -> a -> b) -> b -> t a -> b
  foldr1  :: (a -> a -> a) -> t a -> a
  foldl1  :: (a -> a -> a) -> t a -> a
  toList  :: t a -> [a]
  null    :: t a -> Bool
  length  :: t a -> Int
  elem    :: Eq a => a -> t a -> Bool
  maximum :: Ord a => t a -> a
  minimum :: Ord a => t a -> a
  sum     :: Num a => t a -> a
  product :: Num a => t a -> a
```

besides the abstraction of the `foldr` function, `Foldable` provides several other useful operations when dealing with
*container*-like structures.

Because of the regular structure algebraic data types it is again possible to automatically derive `Foldable` instances
by using the `deriving` clause:

```haskell
{-# LANGUAGE DeriveFunctor, DeriveFoldable #-} -- allows automatic deriving of Functor and Foldable
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Eq, Show, Read, Functor, Foldable)
```

Of course, we can also implement the `foldr` function on our own:

```haskell
instance Foldable Tree where
  foldr f acc (Leaf a)   = f a acc
  foldr f acc (Node a b) = foldr f (foldr f acc b) a
```

We can now use `foldr` and other class methods of `Foldable`:

```haskell
statusTree :: Tree Status
statusTree = Node (Leaf Green) (Node (Leaf Red) (Leaf Yellow))

maxStatus = foldr max Green statusTree
maxStatus' = maximum statusTree

-- using length from Foldable type class
treeSize = length statusTree

-- in GHCi:
λ> :t max
max :: Ord a => a -> a -> a

λ> foldr max Green statusTree
Red
-- using maximum from Foldable type class:
λ> maximum statusTree
Red
λ> treeSize
3
-- using toList from Foldable type class:
λ> toList statusTree
[Green,Red,Yellow]
```

### The Maybe Monad

Now we will take the data type `Maybe` as an example to dive deeper into the more complex parts of the
Haskell type class system.

The `Maybe` type is quite simple, it can be either a null value, called `Nothing` or a value of type `a` 
constructed by `Just a`:

```haskell
data  Maybe a  =  Nothing | Just a deriving (Eq, Ord)
```

The Maybe type is helpful in situations where certain operation *may* return a valid result or not.
Take for instance the function `lookup` from the Haskell base library. It looks up a key in a list of
key-value pairs. If it finds the key, the associated value `val` is returned - but wrapped in a Maybe: `Just val`.
If it doesn't find the key, `Nothing` is returned:

```haskell
lookup :: (Eq a) => a -> [(a,b)] -> Maybe b
lookup _key []  =  Nothing
lookup  key ((k,val):rest)
    | key == k  =  Just val
    | otherwise =  lookup key rest
```

The `Maybe` type is a very simple means that helps to avoid NullPointer errors or similar issues with undefined results.
Thus, many languages have adopted it under different names. In Java for instance, it is called `Optional`.

#### Total functions

In Haskell, it is considered good practise to use *total functions* - that is functions that have defined
return values for all possible input values - where ever possible to avoid runtime errors.

Typical examples for *partial* (i.e. non-total) functions are division and square roots.
We can use `Maybe` to make them total:

```haskell
safeDiv :: (Eq a, Fractional a) => a -> a -> Maybe a
safeDiv _ 0 = Nothing
safeDiv x y = Just (x / y)

safeRoot :: (Ord a, Floating a) => a -> Maybe a
safeRoot x
  | x < 0     = Nothing
  | otherwise = Just (sqrt x)
```

In fact, there are alternative base libraries that don't provide any partial functions.

#### Composition of Maybe operations 

Now let's consider a situation where we want to combine several of those functions. 
Say for example we first want to lookup the divisor from a key-value table, then perform a
division with it and finally compute the square root of the quotient:

```haskell
findDivRoot :: Double -> String -> [(String, Double)] -> Maybe Double
findDivRoot x key map =
  case lookup key map of
      Nothing -> Nothing
      Just y  -> case safeDiv x y of
          Nothing -> Nothing
          Just d  -> case safeRoot d of
              Nothing -> Nothing
              Just r  -> Just r

-- and then in GHCi:
λ> findDivRoot 27 "val" [("val", 3)]
Just 3.0
λ> findDivRoot 27 "val" [("val", 0)]
Nothing
λ> findDivRoot 27 "val" [("val", -3)]
Nothing
```

The resulting control flow is depicted in the following diagram:
![The Maybe railroad](img/maybe.png)

In each single step we have to check for `Nothing`, in that case we directly short circuit to an overall `Nothing` result value.
In the `Just` case we proceed to the next processing step.

This kind of handling is repetitive and buries the actual intention under a lot of boilerplate.
As Haskell uses layout (i.e. indentation) instead of curly brackets to separate blocks the code will
end up in what is called the *dreaded staircase*: it marches to the right of the screen.

So we are looking for a way to improve the code by abstracting away the chaining of functions that return
`Maybe` values and providing a way to *short circuit* the `Nothing` cases.

We need an operator `andThen` that takes the `Maybe` result of a first function
application as first argument, and a function as second argument that will be used in the `Just x` case and again 
returns a `Maybe` result.
In case that the input is `Nothing` the operator will directly return `Nothing` without any further processing.
In case that the input is `Just x` the operator will apply the argument function `fun` to `x` and return its result:

```haskell
andThen :: Maybe a -> (a -> Maybe b) -> Maybe b
andThen Nothing _fun = Nothing
andThen (Just x) fun = fun x
```

We can then rewrite `findDivRoot` as follows:

```haskell
findDivRoot'''' x key map =
  lookup key map `andThen` \y ->
  safeDiv x y    `andThen` \d ->
  safeRoot d
```

(Side note: In Java the `Optional` type has a corresponding method: [Optional.flatmap](https://docs.oracle.com/javase/8/docs/api/java/util/Optional.html#flatMap-java.util.function.Function-))

This kind of chaining of functions in the context of a specific data type is quite common. So, it doesn't surprise us that
there exists an even more abstract `andThen` operator that works for arbitrary parameterized data types:

```haskell
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```

When we compare this *bind* operator with the type signature of the `andThen` operator:

```haskell
andThen :: Maybe a -> (a -> Maybe b) -> Maybe b

```

we can see that both operators bear the same structure.
The only difference is that instead of the concrete type `Maybe` the signature of `(>>=)`
uses a type variable `m` with a `Monad` type class constraint. We can read this type signature as:

For any type `m` of the type class `Monad` the operator `(>>=)` is defined as `m a -> (a -> m b) -> m b`
Based on `(>>=)` we can rewrite the `findDivRoot` function as follows:

```haskell
findDivRoot' x key map =
  lookup key map >>= \y ->
  safeDiv x y    >>= \d ->
  safeRoot d
```

Monads are a central element of the Haskell type class ecosystem. In fact the monadic composition based on `(>>=)` is so
frequently used that there exists some specific syntactic sugar for it. It's called the do-Notation.
Using do-Notation `findDivRoot` looks like this:

```haskell
findDivRoot''' x key map = do
  y <- lookup key map
  d <- safeDiv x y
  safeRoot d
```

This looks quite like a sequence of statements (including variable assignments) in an imperative language.
Due to this similarity Monads have been aptly called [programmable semicolons](http://book.realworldhaskell.org/read/monads.html#id642960).
But as we have seen: below the syntactic sugar it's a purely functional composition!

### Purity

A function is called pure if it corresponds to a function in the mathematical sense: it associates each possible input 
value with an output value, and does nothing else. In particular,

- it has no side effects, that is to say, invoking it produces no observable effect other than the result it returns; 
  it cannot also e.g. write to disk, or print to a screen.
- it does not depend on anything other than its parameters, so when invoked in a different context or at a different 
  time with the same arguments, it will produce the same result.
  

Purity makes it easy to reason about code, as it is so close to mathematical calculus. 
The properties of a Haskell program can thus often be determined with equational reasoning.
(As an example I have provided an [example for equational reasoning in Haskell](functor-proof.md).

Purity also improves testability: It is much easier to set up tests without worrying about mocks or stubs to factor out
access to backend layers.

All the functions that we have seen so far are all *pure* code that is free from side effects.

So how can we achieve side effects like writing to a database or serving HTTP requests in Haskell?

The Haskell language designers came up with a solution that distinguishes Haskell from most other languages:
Side effects are always explicitly declared in the function type signature.
In the next section we will learn how exactly this works.

### Explicit side effects with the IO Monad

> Monadic I/O is a clever trick for encapsulating sequential, imperative computation, so that it can “do no evil” 
> to the part that really does have precise semantics and good compositional properties.
>
> [Conal Elliott](http://conal.net/blog/posts/is-haskell-a-purely-functional-language)

The most prominent Haskell Monad is the `IO` monad. It is used to compose operations that perform I/O.
We'll study this with a simple example.

In an imperative language, reading a String from the console simply returns a String value (e.g. `BufferedReader.readline()` in Java: 
`public String readLine() throws IOException`).

In Haskell the function `getLine` does not return a `String` value but an `IO String`: 

```haskell
getLine :: IO String
```
This could be interpreted as: `getLine` returns a String in an IO context. 
In Haskell, it is not possible to extract the String value from its IO context (In Java on the other hand you could always 
catch away the `IOException`).

So how can we use the result of `getLine` in a function that takes a `String` value as input parameter?

We need the monadic bind operation `(>>=)` to do this in the same as we already saw in the `Maybe` monad:

```haskell
-- convert a string to upper case
strToUpper :: String -> String
strToUpper = map toUpper 
 
up :: IO () 
up = 
  getLine >>= \str ->
  print (strToUpper str)

-- and then in GHCi:
λ> :t print
print :: Show a => a -> IO ()
λ> up
hello world
"HELLO WORLD"
```

or with do-Notation:
```haskell
up' :: IO () 
up' = do
  str <- getLine
  print (strToUpper str)
```

Making side effects explicit in function type signatures is one of the most outstanding achievements of Haskell.
This feature will lead to a very rigid distinction between code that is free of side effects (aka *pure* code) and code 
that has side effects (aka *impure* code).

Keeping domain logic *pure* - particularly when working only with *total* functions - will dramatically improve 
reliability and testability as tests can be run without setting up mocks or stubbed backends.

It's not possible to introduce side effects without making them explicit in type signatures. 
There is nothing like the *invisible* Java `RuntimeExceptions`. 
So you can rely on the compiler to detect any violations of a rule like "No impure code in domain logic".

I've written a simple Restaurant Booking REST Service API that explains how Haskell helps you to keep domain logic pure by
organizing your code according to the [ports and adapters pattern](https://github.com/thma/RestaurantReservation).

The section on type classes (and on Monads in particular) have been quite lengthy. Yet, they have hardly shown more than
the tip of the iceberg. If you want to dive deeper into type classes, I recommend 
[The Typeclassopedia](https://wiki.haskell.org/Typeclassopedia).

## Conclusion

We have covered quite a bit of terrain in the course of this article.

It may seem that Haskell has invented an intimidating mass of programming concepts.
But in fact, Haskell inherits much from earlier functional programming languages.

Features like first class functions, comprehensive list APIs or declarative programming
had already been introduced with Lisp and Scheme.

Several others, like pattern matching, non-strict evaluation, immutability, purity, static and strong typing,
type inference, algebraic data types and polymorphic data types
have been invented in languages like Hope, Miranda an ML.

Only a few features like type classes and explicit side effects / monadic I/O have first been introduced in Haskell.

So if you already know some functional languages, Haskell will not be to alien to you.
For developers with a background in OO languages, the conceptual gap will be much larger.

I hope that this article helped to bridge that gap a bit and to better explain [why 
functional programming](https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf) - and Haskell in particular - matters.

Using functional programming languages - or applying some of its techniques - will help
to create designs that are closer to the problem domain (as intented by domain driven design), 
more readable (due to their declarative character), allow equational reasoning, will provide more rigid
separation of business logic and side effects,
are more flexible for future changes or extensions, provide better testability (supporting BDD, TDD and property based testing), 
will need much less debugging, are better to maintain and, last not least, will be more fun to write.


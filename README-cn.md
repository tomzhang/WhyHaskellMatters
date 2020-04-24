# Why Haskell Matters [英文版](./README.MD)

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
    -  [Maybe操作符的构成](#Maybe操作符的构成)
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

### 列表推导式

你还记得你数学课上设置的推导符号吗？

简单的例子是偶数集的定义：

> Evens = {i | i = 2n ∧ n ∊ ℕ}

它可以理解为：Evens定义为所有“i”的集合，其中“i=2*n”和“n”是自然数集合的一个元素。

Haskell*列表推导*允许我们使用类似的语法定义-潜在的无限-列表：

```haskell
evens' = [2*n | n <- [1..]]
```

同样，我们可以通过只计算“evens”的有限子集来避免无限循环：

```haskell
λ> take 10 evens'
[2,4,6,8,10,12,14,16,18,20]
```

列表推导式对于以（主要是）声明的方式定义数字集和序列非常有用接近最初的数学定义。

以毕达哥拉斯所有三元组的set`PT'为例

>  PT = {(a,b,c) | a,b,c ∊ ℕ ∧ a² + b² = c² }

Haskell的定义如下：

```haskell
pt :: [(Natural,Natural,Natural)]
pt = [(a,b,c) | c <- [1..],
                b <- [1..c],
                a <- [1..b],
                a^2 + b^2 == c^2]
```

### 将控制流结构定义为抽象

在大多数语言中，不可能定义新的条件操作，例如您自己的“myIf”语句。条件运算只有在满足某些条件时才会计算其某些参数。如果不是不可能的话，这很难用一种语言来实现，这种语言的逐值调用语义在
实际评估功能体。

由于Haskell实现了按需调用语义，因此可以定义新的条件操作。事实上，这在编写*领域特定语言*时非常有用。

下面是一个非常简单的“myIf”版本：

```haskell
myIf :: Bool -> b -> b -> b
myIf p x y = if p then x else y 

λ> myIf (4 > 2) "true" viciousCircle
"true"
```

一个更有用的控制结构是源于LISP和Scheme语言的“cond”（用于条件）函数。
它允许您定义更类似于表的决策结构，有点类似于C风格语言中的“switch”语句：

```haskell
cond :: [(Bool, a)] -> a
cond []                 = error "make sure that at least one condition is true"
cond ((True,  v):rest)  = v
cond ((False, _):rest)  = cond rest
```

使用此函数，我们可以实现signum函数“sign”，如下所示：

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

## 类型类

现在我们来看看Haskell最显著的特性之一：*类型类*。

在这一节 [多态数据类型](#多态数据类型) 我们已经看到类型变量（或参数）允许要多态的类型声明，如：

```haskell
data [a] = [] | a : [a]
```

这种方法被称为*参数多态性*，并在几种编程语言中使用。

另一方面，类型类处理数据类型的特殊多态性。这种方法也被称为*重载*。

为了获得第一直觉，让我们从一个简单的例子开始。

```haskell
λ> 'A' + 25
'Z'

-- please note that in Haskell a string is List of characters: type String = [Char]
λ> map (+ 5) "hello world"
"mjqqt%|twqi"

λ> map (\c -> c - 5) "mjqqt%|twqi"
"hello world"
```

要实现这一点，我们将不得不*重载*中缀运算符“（+）”和“（-”，以便不仅处理数字，而且处理字符。
现在，让我们看看`（+）`运算符的类型签名：

```haskell
λ> :type (+)
(+) :: Num a => a -> a -> a
```

因此，“（+）”不仅被声明为类型“（+）：：a->a->a”，而且它在类型变量“a”上包含一个**约束**，
即“Num a=>”。
“（+）”的整个类型签名可以读取为：对于属于类型类“Num”成员的所有类型“a”，运算符“（+）”具有
`a -> a -> a`.

接下来，我们将获得有关类型类“Num”的更多信息：

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

此信息详细说明了类型“a”必须实现哪些函数才能用作“Num”类型类的实例。
该行{-#MINIMAL+，（*）、abs、signum、fromInteger，（negate |（-）#-}告诉我们最小的完整实现是什么
必须提供。它还告诉我们，“Word”、“Integer”、“Int”、“Float”和“Double”类型是“Num”类型类的实例。

这是使类型“Char”成为“Num”类型类的实例所需知道的全部内容，因此无需进一步的ado
深入到实现中（请注意“fromEnum”将“Char”转换为“Int”和“toEnum”并将“Int”转换为“Char”：

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

这段代码使类型“Char”成为“Num”类型类的实例。然后我们可以使用上面演示的“（+）”和“-”。

最初，类型类的想法是提供算术运算符的重载以便在所有数值类型中使用相同的运算符。

但是类型类的概念在其他许多情况下也被证明是有用的。这导致Haskell基库提供了丰富的类型类集，并且
利用这个强大概念的大量编程技术。

下面是Haskell库中一些最重要类型类的图形概述：

![The hierarchy of basic type classes](https://upload.wikimedia.org/wikipedia/commons/thumb/0/04/Base-classes.svg/510px-Base-classes.svg.png)

我不会全部都过一遍，但我会报道一些最重要的。

让我们从Eq开始：

```haskell
class  Eq a  where
   (==), (/=) :: a -> a -> Bool

       -- Minimal complete definition:
       --      (==) or (/=)
   x /= y     =  not (x == y)
   x == y     =  not (x /= y)
```

这个定义说明了两件事：

- 如果类型“a”要成为类“Eq”的实例，则它必须支持函数`（==）`和`（/=）`都具有类型“a->a->Bool”。

- ‘Eq’为`（==）`和`（/=）`提供了*默认定义*。

  因此，不需要'Eq'中的类型提供这两个定义-给定其中一个，另一个将自动工作。

现在，我们可以在[代数数据类型](#代数数据类型) 中导入“Eq”类型类的实例。

这里的类型声明是一个总结：

```haskell
data Status   = Green | Yellow | Red
data Severity = Low | Middle | High 
data PairStatusSeverity = PSS Status Severity
```

首先，我们为简单类型'Status'和'Severity'创建Eq实例，方法是定义`（==）`操作符：

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

接下来，我们通过定义`（=）`运算符为` PairStatusSeverity`创建一个'Eq'实例：

```haskell
instance Eq PairStatusSeverity where
   (PSS sta1 sev1) == (PSS sta2 sev2) = (sta1 == sta2) && (sev1 == sev2)
```

有了这些定义，现在可以在我们的三种类型上使用`（==）`和`（/=）'。正如您将注意到的，实现“Eq”的代码相当无聊。即使是机器也能做到！

这就是为什么语言设计者提供了一个“派生”机制，让编译器自动实现如果类实例可以像“Eq”那样自动派生，请键入该实例。

使用此语法，更容易让类型实现“Eq”类型类：

```haskell
data Status   = Green | Yellow | Red          deriving (Eq)
data Severity = Low | Middle | High           deriving (Eq)
data PairStatusSeverity = PSS Status Severity deriving (Eq)
```

这种类型类实例的自动派生在许多情况下都有效，并减少了大量的重复代码。

例如，它可以自动派生“Ord”类型类的实例，该类提供订购功能：

```haskell
class  (Eq a) => Ord a  where
    compare              :: a -> a -> Ordering
    (<), (<=), (>), (>=) :: a -> a -> Bool
    max, min             :: a -> a -> a
    ...
```

如果对“Status”和“Severity”类型使用“dering”，编译器将实现根据类型声明中构造函数的顺序排序。
那就是 `Green < Yellow < Red` and `Low < Middle < High`:

```haskell
data Status   = Green | Yellow | Red          deriving (Eq, Ord)
data Severity = Low | Middle | High           deriving (Eq, Ord)
```

### 读取和展示

另外两个非常有用的类型类是“Read”和“Show”，它们也支持自动派生。

`Show`提供具有以下类型签名的函数'Show'：

```haskell
show :: Show a => a -> String
```

这意味着实现“Show”的任何类型都可以转换（或*封送处理*）为“String”表示形式。
“Show”实例的创建可以通过在类型声明中添加“derivering（Show）”子句来实现。

```haskell
data PairStatusSeverity = PSS Status Severity deriving (Show)

λ> show (PSS Green Low)
"PSS Green Low"
```

“Read”类型类用于执行相反的操作：*使用函数“Read”从字符串中解组*数据：

```haskell
read :: Read a => String -> a
```

此签名表示，对于实现“Read”类型类的任何类型“a”，函数“Read”都可以从字符串表示重建“a”的实例：

```haskell
data PairStatusSeverity = PSS Status Severity deriving (Show, Read)
data Status = Green | Yellow | Red            deriving (Show, Read)
data Severity = Low | Middle | High           deriving (Show, Read)

λ> marshalled = show (PSS Green Low)

λ> read marshalled :: PairStatusSeverity
PSS Green Low
```

请注意，需要用“：：PairStatusSeverity”子句指定预期的目标类型。Haskell使用静态编译时类型。在编译时无法确定哪种类型将返回“read”some string content“表达式。因此，必须在编译时指定所需的类型。
通过某个函数类型签名给出的隐式声明，或者如上所示，通过明确的声明。

show和read一起提供了序列化（marshal）和反序列化（unmarshal）Haskell的方便方法数据结构。
此机制不提供任何优化的二进制表示，但对于在许多实际应用中，格式比JSON更紧凑，而且不需要解析器库。

### 函子与可折叠

最有趣的类型类是从抽象代数或范畴理论派生出来的。研究它们是一个非常有益的过程，我强烈推荐。不过，这绝对是超出了本文的范围。因此，我只指出了两个关于Haskell这一部分的资源键入类层次结构。
第一个是传奇 [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia) 布伦特·约吉. 
第二个是 [Lambda 终极样板工厂](https://github.com/thma/LtuPatternFactory)   
本文将代数类型类与软件设计模式联系起来。

所以我们将只讨论其中一些类型的类。

在 [声明式编程](#声明式编程) 的章节中，we came across to very useful concepts:

- 将函数映射到列表的所有元素上 (`map :: (a -> b) -> [a] -> [b]`)
- 使用二进制操作和该操作的中性（标识）元素减少列表(`foldr :: (a -> b -> b) -> b -> [a] -> b`)

这些概念不仅对列表有用，而且对许多其他数据结构也有用。所以它并不奇怪的是，有类型类抽象了这些概念。

#### 函子

“Functor”类型类泛化了将函数应用于上下文中的值而不更改上下文的功能，（例如，将函数映射到列表`[a]`上，该列表`[b]`返回相同长度的新列表`[a]'）：

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

让我们通过玩一个简单的二叉树来进一步了解这个想法：

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

我们希望使用函数“to Severity：：Status->Severity”转换“statusTree”的所有`Status'元素进入“Severity”实例。

因此，我们让'Tree'实例化'Functor'类：

```haskell
instance Functor Tree where
  fmap f (Leaf a)   = Leaf (f a)
  fmap f (Node a b) = Node (fmap f a) (fmap f b)
```


我们现在可以在“Tree”数据结构上使用“fmap”：

```haskell
λ> fmap toSeverity statusTree
Node (Leaf Low) (Node (Leaf High) (Leaf Middle))
λ> :type it
it :: Tree Severity
```

如上所述，fmap保持树结构不变，但转换每个“Leaf”元素的类型，有效地将树的类型更改为“Tree Severity”。

由于派生“Functor”实例是一项枯燥的任务，因此再次可以使用“dering”子句来让数据类型实例化“Functor”：

```haskell
{-# LANGUAGE DeriveFunctor #-} -- this pragma allows automatic deriving of Functor instances
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, Functor)
```

#### 可折叠

如前所述，“Foldable”提供了对实例化`可折叠`type类：

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

除了“foldr”函数的抽象之外，“Foldable”在处理*容器类结构。

由于规则结构的代数数据类型，再次可以自动派生“可折叠”实例通过使用“dering”子句：

```haskell
{-# LANGUAGE DeriveFunctor, DeriveFoldable #-} -- allows automatic deriving of Functor and Foldable
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Eq, Show, Read, Functor, Foldable)
```

当然，我们也可以自己实现“foldr”功能：

```haskell
instance Foldable Tree where
  foldr f acc (Leaf a)   = f a acc
  foldr f acc (Node a b) = foldr f (foldr f acc b) a
```

我们现在可以使用“foldr”和“Foldable”的其他类方法：

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

现在我们将以数据类型“Maybe”为例，深入了解Haskell类型的类系统。

“Maybe”类型非常简单，它可以是空值、名为“Nothing”或类型为“a”的值`由“Just a”构造：

```haskell
data  Maybe a  =  Nothing | Just a deriving (Eq, Ord)
```

Maybe类型在某些操作*可能*返回有效结果或不返回有效结果的情况下非常有用。以Haskell库中的函数'lookup'为例。它在一个键值对。如果找到键，则返回关联的值“val”，但包装在Maybe:“Just val”中。
如果找不到key，则返回“Nothing”：

```haskell
lookup :: (Eq a) => a -> [(a,b)] -> Maybe b
lookup _key []  =  Nothing
lookup  key ((k,val):rest)
    | key == k  =  Just val
    | otherwise =  lookup key rest
```

“Maybe”类型是一种非常简单的方法，有助于避免空指针错误或具有未定义结果的类似问题。
因此，许多语言以不同的名称采用了它。例如，在Java中，它被称为“可选”。

#### Total 函数

在Haskell中，使用*total functions*-即定义了返回所有可能输入值的值-尽可能避免运行时错误。

*偏*（即非全）函数的典型例子是除法和平方根。我们可以用“Maybe”来表示它们的总数：

```haskell
safeDiv :: (Eq a, Fractional a) => a -> a -> Maybe a
safeDiv _ 0 = Nothing
safeDiv x y = Just (x / y)

safeRoot :: (Ord a, Floating a) => a -> Maybe a
safeRoot x
  | x < 0     = Nothing
  | otherwise = Just (sqrt x)
```

事实上，有一些替代的基本库不提供任何部分函数。

#### Maybe操作符的构成

现在让我们考虑这样一种情况：我们希望合并其中的几个函数。例如，我们首先要从键值表中查找除数，然后执行
用它除法，最后计算商的平方根：

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

结果控制流如下图所示：
![The Maybe railroad](img/maybe.png)

在每一个步骤中，我们都必须检查“Nothing”，在这种情况下，我们直接对“Nothing”结果值进行短路处理。
在“公平”的情况下，我们进入下一个处理步骤。

这种处理是重复的，把实际意图埋在许多样板下。由于Haskell使用布局（即缩进）而不是花括号来分隔块，代码将最后进入所谓的“可怕的阶梯”：它向屏幕的右侧行进。

因此，我们正在寻找一种方法，通过抽象出返回的函数链来改进代码`也许“值”和提供了一种方法来“短路”那些“无”的情况。

我们需要一个运算符，它接受第一个函数的“可能”结果。应用程序作为第一个参数，函数作为第二个参数，将在'Just x'情况下反复使用返回“Maybe”结果。
如果输入为“Nothing”，则运算符将直接返回“Nothing”，而无需进一步处理。如果输入为“Just x”，则运算符将参数函数“fun”应用于“x”，并返回其结果：

```haskell
andThen :: Maybe a -> (a -> Maybe b) -> Maybe b
andThen Nothing _fun = Nothing
andThen (Just x) fun = fun x
```

然后我们可以重写“findDivRoot”如下：

```haskell
findDivRoot'''' x key map =
  lookup key map `andThen` \y ->
  safeDiv x y    `andThen` \d ->
  safeRoot d
```

(附带说明：在Java中，“Optional”类型有一个对应的方法: [Optional.flatmap](https://docs.oracle.com/javase/8/docs/api/java/util/Optional.html#flatMap-java.util.function.Function-))

这种在特定数据类型的上下文中链接函数的方式非常常见。所以，这并不奇怪，
还有一个更抽象的“and”运算符可用于任意参数化数据类型：

```haskell
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```

当我们将此*bind*运算符与“and”运算符的类型签名进行比较时：

```haskell
andThen :: Maybe a -> (a -> Maybe b) -> Maybe b

```

我们可以看到这两个运算符具有相同的结构。唯一的区别是，与具体类型“Maybe”不同的是，“Maybe”的签名（>>=）`使用带有“Monad”类型类约束的类型变量“m”。我们可以将此类型签名读为：

对于类型类“Monad”的任何类型“m”，运算符“（>>=）”定义为 `m a -> (a -> m b) -> m b`
基于 `(>>=)` 我们可以将 `findDivRoot` 函数写成:

```haskell
findDivRoot' x key map =
  lookup key map >>= \y ->
  safeDiv x y    >>= \d ->
  safeRoot d
```

单子是Haskell类生态系统的核心元素。实际上，基于`（>='）的一元构图是这样的。经常使用的是它有一些特定的语法糖。它叫做do符号。
使用do符号“findDivRoot”如下：

```haskell
findDivRoot''' x key map = do
  y <- lookup key map
  d <- safeDiv x y
  safeRoot d
```

这看起来很像命令式语言中的一系列语句（包括变量赋值）。
由于这种相似性，单子被恰当地称为 [可编程分号](http://book.realworldhaskell.org/read/monads.html#id642960).
但正如我们所看到的：在句法糖的下面，它是一个纯粹的功能组合！

### 纯净性

如果函数与数学意义上的函数相对应，则称之为纯函数：它将每个可能的输入关联起来
有输出值的值，而不做其他任何事情。特别地，

- 它没有副作用，也就是说，调用它除了返回的结果之外，不会产生任何可观察的效果；
  它也不能写入磁盘或打印到屏幕。
- 它不依赖于除参数以外的任何东西，因此当在不同的上下文中或在不同的上下文中调用时
  如果参数相同，则会产生相同的结果。

纯粹性使我们很容易对代码进行推理，因为它与数学微积分非常接近。
因此，Haskell程序的属性通常可以通过等式推理来确定。
（作为一个例子，我提供了一个 [Haskel中的等式推理实例](functor-proof.md).

纯粹性还提高了可测试性：不必担心模型或存根的因素，就可以更容易地设置测试访问后端层。

到目前为止，我们看到的所有函数都是纯代码，没有任何副作用。

那么，我们如何在Haskell中实现诸如写入数据库或服务HTTP请求之类的副作用呢？

Haskell语言设计师提出了一个解决方案，将Haskell与大多数其他语言区分开来：

副作用总是在函数类型签名中显式声明的。
在下一节中，我们将了解这是如何工作的。

### IO-Monad的显性副作用

> 一元I/O是封装顺序的、命令式计算的聪明技巧，因此它可以对于真正有精确语义和良好的组合特性的部分“无恶不作”。
> 
>[Conal Elliott](http://conal.net/blog/posts/is-haskell-a-purely-functional-language)

最著名的Haskell单子是“IO”单子。它用于组合执行I/O的操作。
我们将用一个简单的例子来研究这个问题。

在命令式语言中，从控制台读取字符串只需返回字符串值(e.g. Java中的`BufferedReader.readline()` : 
`public String readLine() throws IOException`).

在Haskell中，“getLine”函数不返回“String”值，而是返回“IO String”：

```haskell
getLine :: IO String
```
这可以解释为：`getLine`在IO上下文中返回一个字符串。
在Haskell中，不可能从其IO上下文中提取字符串值（另一方面，在Java中，您可以始终
抓住“IOException”）。

那么，如何在一个函数中使用“getLine”的结果，该函数将“String”值作为输入参数？

我们需要一元绑定操作“（>>=）”来执行此操作，就像我们在“Maybe”一元中已经看到的那样：

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

或使用do符号：
```haskell
up' :: IO () 
up' = do
  str <- getLine
  print (strToUpper str)
```

在函数类型签名中明确副作用是Haskell最杰出的成就之一。
这个特性将导致在没有副作用的代码（也就是纯代码）和代码之间有一个非常严格的区别有副作用（又称不洁代码）。

保持域逻辑*pure*-特别是当只使用*total*函数时-将显著提高可靠性和可测试性，因为测试可以在不设置模型或存根后端的情况下运行。

如果不在类型签名中明确说明它们，就不可能引入副作用。没有什么比不可见的Java“RuntimeExceptions”更合适的了。因此，您可以依赖编译器来检测任何违反规则的行为，比如“域逻辑中没有不纯代码”。

我已经编写了一个简单的餐厅预订REST服务API，它解释了Haskell如何帮助您通过组织你代码根据 [ports and adapters pattern](https://github.com/thma/RestaurantReservation).

关于类型类（尤其是monad）的部分相当长。然而，他们几乎没有表现出超过冰山一角。如果你想深入研究类型类，我建议你读[The Typeclassopedia](https://wiki.haskell.org/Typeclassopedia).

## 总结

在本文中，我们已经介绍了相当多的内容。

Haskell似乎发明了一大堆令人生畏的编程概念。但事实上，Haskell继承了早期函数式编程语言的许多优点。

一类函数、综合列表api或声明式编程等功能已经引入了Lisp和Scheme。

还有一些，比如模式匹配，非严格评估，不变性，纯度，静态和强类型，类型推理、代数数据类型和多态数据类型
已经被发明的语言像Hope，Miranda和ML。

Haskell中只首次引入了一些特性，如类型类和显式的副作用/一元I/O。所以如果你已经知道一些函数式语言，Haskell对你来说不会陌生。对于有面向对象语言背景的开发人员来说，概念上的差距会更大。

我希望这篇文章有助于缩小这一差距，并更好地解释 [why functional programming](https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf) - 和  Haskell in particular - 尤其重要.

使用函数式编程语言——或者应用它的一些技术——将有助于为了创建更接近问题领域的设计（正如领域驱动设计所期望的那样），更具可读性（由于它们的声明性），允许等式推理，将提供更严格的
分离业务逻辑和副作用，对于未来的更改或扩展更加灵活，提供更好的可测试性（支持BDD、TDD和基于属性的测试），将需要更少的调试，更好的维护，最后，将更有趣的编程。


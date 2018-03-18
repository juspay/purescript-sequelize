# purescript-sequelize
Purescript wrapper for Sequelize - http://docs.sequelizejs.com/

## LICENCE
https://github.com/juspay/purescript-sequelize/blob/master/LICENSE

## Usage

### Connections

Before we can do anything, we need to have configuration information for our
database.
That means things like database type (SQLite, PostgreSQL, etc), username/password
(if any), port, and so on.

In JS that would be done like so:

```javascript
const Sequelize = require('sequelize');
const sequelize = new Sequelize({
  database: "myAwesomeDatabase",
  dialect: "sqlite",
  storage: "./awesome.sqlite"
});
```

In PS, the type of `sequelize` would be `Conn`.
There is one way to get a value of this type:

```purescript
getConn
  :: forall e
   . Options ConnOpts
  -> Aff ( sequelize :: SEQUELIZE | e ) Conn
```

And we can use it like so:

```purescript
myConn :: forall e. Aff (sequelize :: SEQUELIZE | e) Conn
myConn = getConn opts
  where
    opts = database := "myAwesomeDatabase"
        <> dialect := SQLite
        <> storage := "./awesome.sqlite"
```

Now that we have a connection to our database, we can start defining our tables
via Sequelize's concept of a model.

### Models

In PS, we use data types to represent Sequelize models.
Let's say we have a database of users:

```purescript
newtype User = User { name :: String, age :: Int }
```

This means we want to have a table in the database for all our users, with
one row that holds their name.

Sequelize lets you define models using objects where keys are table columns names
and values are options on those columns:

```javascript
const user = sequelize.define("user", {
  name: {
    type: Sequelize.STRING(255),
    defaultValue: "me"
  },
  age: {
    type: Sequelize.INTEGER(255),
    defaultValue: 20
  }
},
  {}
);
```

In order to pass an object like this to Sequelize, we'll need to transform
any value of type `User` to a `Foreign` js object.
To that end, we'll derive some instances:

```purescript
derive instance eqUser :: Eq User

derive instance genericUser :: Generic User _

instance showUser :: Show User where
  show = genericShow

instance decodeUser :: Decode User where
  decode x = genericDecodeModel x

instance encodeUser :: Encode User where
  encode x = genericEncodeModel x

instance encodeModelUser :: EncodeModel User where
  encodeModel x = genericEncodeModel x

instance decodeModelUser :: DecodeModel User where
  decodeModel x = genericDecodeModel x
```

The `EncodeModel` class is what lets Sequelize understand our PS `User` type.

But recall that in our JS example, we defined a default value for the `name`
column and told Sequelize its type (`STRING(255)`). We also passed it a string
that tells Sequelize what name to store the table as:

```purescript
instance isModelUser :: Model User where
  modelCols _ = userCols
  modelName _ = "user"

userCols :: ModelCols
userCols = ["name" /\ nameOpts, "age" /\ ageOpts]
  where
  nameOpts =
    columnType := ModelTypes.String {length: Nothing} <>
    defaultValue := toForeign "me"
  ageOpts =
    columnType := ModelTypes.Integer {length: Nothing} <>
    defaultValue := toForeign 20
```

Note the catch-all pattern match in the `isModelUser` instance: this is because
we need a proxy to tell the compiler how to pick an appropriate definition for
our columns definition and name. If we didn't have that proxy, there wouldn't be
any way to tell that the string "user" and the options for our columns correspond
with our `User` type!

Remember that in JS-land we had `const user = sequelize.define(...)`.
In PS, that means `user` would have type `ModelOf User`.
There is one way of getting a `ModelOf a`, for any `a` with an instance of `Model`:

```purescript
makeModelOf
  :: forall a e. Model a
  => Conn
  -> Options ModelOpts
  -> Aff ( sequelize :: SEQUELIZE | e ) (ModelOf a)
```

therefore we can get Sequelize's version of our model as in the following:

```purescript
getModelOfUser :: forall e. Aff ( sequelize :: SEQUELIZE | e ) (ModelOf User)
getModelOfUser = do
  conn <- myConn
  user <- makeModelOf conn mempty -- we don't want to pass any extra options
  syncConn conn {force: true} -- drop the current table and recreate it
  pure user
```

### Inserting rows to our table

Suppose we have our hands on a concrete `User` value, for example from a
registration form on our app's frontend:

```purescript
user :: User
user = User {name: "Jonas Valanciunas", age: 26}
```

Sequelize lets you do this in two general ways: by building a value that then
gets saved to the database, or creating it all at one time (which is equivalent
to building and then saving immediately).

Let's assume we're creating our user in one step. Then we can use:

```purescript
create
  :: forall a b e
   . Submodel a b
  => ModelOf a
  -> b
  -> Aff ( sequelize :: SEQUELIZE | e ) (Instance b)
```

The `Submodel` class is just a relation between two models. For example, we
might also want to obtain our user's ID from the database, but there's no way
to do that since if we try to use the `DecodeModel` instance, we can't get any
information back besides what our user's name is. Hence, we can use a supermodel
that includes the name _and_ the ID.

There is an instance for reflexivity and a function for transitivity: that is,
if `a` has a `Model` instance then it's already a `Submodel` of itself, and if
`a` is a submodel of `b` which is a submodel of `c`, then `a` is already a
submodel of `c`. (Transitivity is not provided as an instance because it's very
easy to confuse the compiler as to which instance to pick).

### Retrieving data

Now we want to fetch data from our table. Note that `create` gave us access to
a value of type `Instance User`: this is just an opaque type representing
Sequelize's notion of an instance, that is, what happens after using queries or
something like `create`. In fact, all our queries in PS give us access to an
`Instance User`, like the following:

```purescript
findOne
  :: forall a b e. Submodel a b
  => ModelOf a
  -> Options b
  -> Aff ( sequelize :: SEQUELIZE | e ) (Maybe (Instance b))
```

Since this type is opaque, we need a way to go from an `Instance User` to a
regular `User`. We can use `instanceToModel`:

```purescript
instanceToModel
  :: forall a. Model a
  => Instance a
  -> F a
```

After handling any possible errors from trying to deserialize our `Instance`, we
can use our `User` value.

### Using "Where"

There are two different ways to construct a "where" clause in a query. In JS we
might do something like the following:

```javascript
user.findAll({
  where: {
    age: 20,
  }
});
```

In this example, we're matching an exact value: we want only the users whose age
is exactly 20.

Or we may do something a bit more complicated:

```javascript
user.findAll({
  where: {
    age: {
      $gte: 20
    }
  }
});
```

In this case, we want only those users whose ages are strictly larger than 19.

#### `WHERE`

This is the simplest construction to use. All it does is let you match literal
values in the row; it's the equivalent of the first JS example above:

```purescript
simpleWhere :: WHERE User
simpleWhere = WHERE [Tuple "age" (Int 20)]
```

#### `Where`

This is the more expressive construction. To write the equivalent of the second
JS example:

```purescript
moreComplicatedWhere :: Where User
moreComplicatedWhere = "age" $>= 20
```

We can construct more complicated clauses by using the boolean `$&` and `$|`
combinators:

```purescript
evenMoreComplicatedWhere :: Where User
evenMoreComplicatedWhere = "age" $>= 20 $& "age" $<= 30
```

And for more than two clauses conjoined by a boolean combinator, you can directly
use the type constructors for `Where`:

```purescript
nestedBooleanWhere :: Where User
nestedBooleanWhere = Or
  [ "age" $>= 20 $& "age" $<= 30
  , "name" $<- [String "Pau Gasol", String "Marc Gasol"]
  ]
```

which will match any users with ages between 20 and 30 or whose names are "Pau Gasol"
or "Marc Gasol".


### Free monad DSL

You can also use the free monad DSL from `Sequelize.Free`, which is parameterized
by input and output types. It comes with a default interpreter:

```purescript
runCRUD :: _ => ModelOf a -> CRUD a b ~> Aff (sequelize :: SEQUELIZE | e)
```

as well as all the usual CRUD operations. You could use this for e.g. mocking,
in which case you will need to define your own interpreter and switch it out
in the appropriate place.

## Installing

```
$ npm install --save config
$ bower install --save purescript-sequelize
```

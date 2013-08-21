module Category public

class category
    public
        dual : category
        id
        (.)
    with
        f . g = g ._dual f
        f ._dual g = g . f
    partial
        (.) <-> (._dual)
    unifies
        dual.id = id
        dual.dual = this
    ensures
        forall f.  id . f = f
        forall f.  f . id = f
        forall f g h. (f . g) . h = f . (g . h)
 
class small-category category with
  id : functor
  id.map f = f
  _ . _ : functor
  f . g .map = f.map ._Kata g.map

data Cat : small-category category
 
class product category
    public
        left-category : category
        right-category : category
    with
        id = (left-category.id,right-category.id)
        (f1,f2) . (g1,g2) = (f1 ._left-category g1,  f2 ._right-category g2)
        dual : product category
    unifies
        dual.left-category = left-category.dual
        dual.right-category = right-category.dual

class morphism
    public
        dual : morphism
        arrow
    unifies
        dual.arrow = arrow
        dual.dual = this

class optimized morphism
    public
        carrier : category
    with
        dual : optimized morphism
    unifies
        dual.carrier = carrier.dual
    protected
        open carrier : category

class monomorphism = monic morphism
    -- unenforceable rule forall h g. arrow . g = arrow . h => h = g

class section = split monomorphism
    public
        left-inverse-arrow
    with
        dual : retraction
    unifies
        dual.right-inverse-arrow = left-inverse-arrow

class optimized section
    ensures
        left-inverse-arrow . arrow = id

class epimorphism = epic morphism
    -- unenforceable rule forall h g. g . arrow = h . arrow => h = g

class retraction = split epimorphism
    public
        right-inverse-arrow
    with
        dual : section 
    unifies
        dual.left-inverse-arrow = right-inverse-arrow

class optimized retraction
    ensures
        arrow . right-inverse-arrow = id

class bimorphism = epic monic morphism

class isomorphism = split bimorphism
    public
        inverse-arrow
    unifies
        inverse-arrow = left-inverse-arrow = right-inverse-arrow

class natural isomorphism

class groupoid : category
    public
        dual : groupoid
        invert
    unifies
        dual.invert = invert
    ensures
        forall x. invert (invert x) = x

class functor
    public
        dual : functor
        map
    unifies
        dual.dual = this
        dual.map = map 

class optimized functor
    public
        domain : category
        codomain : category
    with
        dual : optimized functor
    unifies
        dual.domain = codomain
        dual.codomain = domain
    ensures
        forall f g. map f ._codomain map g = map (f ._domain g)
        map domain.id = codomain.id

class endofunctor : functor with
    dual : endofunctor

class optimized endofunctor
    public
        carrier
    unifies
        carrier = domain = codomain
    with
        dual : optimized endofunctor
    protected
        open carrier : category

class simple endofunctor : optimized endofunctor
    with
        dual : simple endofunctor
        carrier = K
    public
        (<$>)
    alias
        (<$>) = map

class pointed functor : endofunctor
    public
        return
    with
        dual : copointed functor
    unifies
        dual.extract = return

class applicative functor
    public
        ap
    with
        dual : coapplicative functor
    unifies
        dual.coap = ap

class simple applicative functor
    public
        (<*>)
    alias
        ap = (<*>)

{-
class optimized applicative functor
    public
        static : category
	-- f (b -> c) -> f (a -> b) -> f (a -> c)
-}

class monad : pointed functor
    public
        bind
        join
    with
        dual : comonad
    unifies
        dual.extend = bind
        dual.duplicate = join

class optimized monad : pointed functor
    public
        dual : optimized comonad
        kleisli : category
        map f = bind (return . f)
        join = bind id
        bind f = join . map f
    partial
        map join -> bind
        bind -> map join
    unifies
        kleisli.id = return
        f ._kleisli g = bind f . g
    ensures
        bind return = id
        forall f.   bind f . return = f
        forall f g. bind f . bind g = bind (bind g . f)

class simple monad : applicative functor
    with
        ap f x = bind (K.flip map x) f

class coapplicative functor
    public
        coap
    with
        dual : applicative functor
    unifies
        coap = dual.ap

class copointed functor : endofunctor
    public
        extract
    with
        dual : pointed functor
    unifies
        dual.return = extract
 
class comonad : copointed functor
    public
        extend
        duplicate
    with
        dual : monad
    unifies
        dual.bind = extend
        dual.join = duplicate

class optimized comonad
    public
        cokleisli : category
    with
        dual : optimized monad
        map f = extend (f . extract)
        duplicate = extend id
        extend f = map f . duplicate
    partial
        map duplicate -> extend
        extend -> map extract
    unifies
        cokleisli.id = extract
        f ._cokleisli g = f . extend f
    ensures
        extend extract = id
        forall f.   extract . extend f = f
        forall f g. extend f . extend g = extend (f . extend g)

class bifunctor : functor
    public
        bimap x y = map (x,y)
    with
        map (x,y) = bimap x y
    partial
        bimap <-> map
 
class optimized bifunctor
    public
        left-domain : category
        right-domain : category
        first f = bimap f right-domain.id
        second = bimap left-domain.id
        left-section : optimized functor
        right-section : optimized functor
    with
        domain : product category
    unifies
        left-section.map = first
        left-section.domain = domain.left-category = left-domain
        left-section.codomain = right-section.codomain = codomain
        right-section.map = second
        right-section.domain = domain.right-category = right-domain
    ensures
        forall f g. first f ._codomain first g = first (f ._left-domain g)
        forall f g. second f ._codomain second g = second (f ._right-domain g)
        forall f g. first f ._codomain second g = bimap f g
        forall f g. second g ._codomain first f = bimap f g

class biendofunctor : bifunctor

class optimized biendofunctor
    public
        carrier
    unifies
        carrier = left-domain = right-domain
    protected
        open carrier : category

class simple bifunctor : optimized biendofunctor with carrier = K

class associative bifunctor : biendofunctor
    public
        associate; disassociate
        associativity : natural isomorphism
    unifies
        associativity.arrow = associate
        associativity.inverse-arrow = disassociate

class optimized associative bifunctor
    ensures
        second associate . associate . product.first associate = associate . associate

class strict associative bifunctor : optimized associative bifunctor
    unifies id = associate = disassociate

class monoidal bifunctor : associative bifunctor
    public
        idl; coidl
        left-identity : natural isomorphism
        idr; coidr
        right-identity : natural isomorphism
    unifies
        left-identity.arrow = idl
        left-identity.inverse-arrow = coidl
        right-identity.arrow = idr
        right-identity.inverse-arrow = coidr

class optimized monoidal bifunctor
    ensures
        associate . product.second idl = product.first idr

class strict monoidal bifunctor : optimized monoidal bifunctor
    unifies id = idl = coidl = idr = coidr

class braided bifunctor
    public
        braid
   
class optimized braided monoidal bifunctor
    ensures
        associate . braid . associate = second braid . associate . first braid
        disassociate . braid . disassociate = first braid . disassociate . second braid
        idr . braid = idl
        idl . braid = idr
        braid . coidr = coidl
        braid . coidl = coidr

class symmetric bifunctor : braided bifunctor

class optimized symmetric bifunctor
    ensures
        braid ._codomain braid = codomain.id

class associative category
    public
        product : optimized associative bifunctor
    with
        dual : coassociative category
    unifies
        product.carrier = this
        product.dual = dual.coproduct

class coassociative category
    public
        coproduct : optimized associative bifunctor
    with
        dual : associative category
    unifies
        coproduct.carrier = this
        coproduct.dual = dual.product

class monoidal category : associative category
    with
        product : optimized monoidal bifunctor
        dual : comonoidal category

class comonoidal category : coassociative category
    with
        coproduct : optimized monoidal bifunctor
        dual : monoidal category

class strict monoidal category
    with product : strict monoidal bifunctor

class strict comonoidal category
    with coproduct : strict monoidal bifunctor

class braided associative category
    with
        product : braided associative bifunctor
        dual : braided coassociative category
    unifies
        dual.coproduct.braid = product.braid

class braided coassociative category
    with
        coproduct : braided associative bifunctor
        dual : braided associative category
    unifies
        dual.product.braid = coproduct.braid 

class symmetric associative category
    with
        product : symmetric associative bifunctor

class symmetric coassociative category
    with
        coproduct : symmetric coassociative bifunctor

class pre-cartesian category : symmetric associative category
    public
        fst
        snd
        diag
        (&&&)
    with
        dual : pre-cocartesian category
        product.bimap f g = (f . fst) &&& (g . snd)
        product.braid f g = snd &&& fst
        product.associate  = (fst . fst) &&& first snd
        product.disassociate = braid . second braid . associate . first braid . braid
    unifies
        dual.inl = fst
        dual.inr = snd
        dual.codiag = diag
        dual.||| = (&&&)
    private
        braid = product.braid
        first = product.first
        second = product.second
        associate = product.associate
    ensures
        fst . diag = id
        snd . diag = id
        forall f g. fst . (f &&& g) = f
        forall f g. snd . (f &&& g) = g

class pre-cocartesian category : symmetric coassociative category
    public
        inl
        inr
        codiag
        (|||)
    with
        coproduct.bimap f g = (inl . f) ||| (inr . g)
        coproduct.braid = inr ||| inl
        coproduct.associate = braid . first braid . disassociate . second braid . braid
        coproduct.disassociate = (inl . inl) ||| first inr
    unifies
        inl = dual.fst
        inr = dual.snd
        codiag = dual.diag
        (|||) = dual.&&&
    private
        braid = coproduct.braid
        first = coproduct.first
        second = coproduct.second
        disassociate = coproduct.disassociate
    ensures
        codiag . inl = id
        codiag . inr = id
        forall f g. (f ||| g) . inl = f
        forall f g. (f ||| g) . inr = g

class cartesian category = monoidal pre-cartesian category
    unifies
        snd = product.idl
        fst = product.idr

class cocartesian category = comonoidal pre-cocartesian category
    unifies
        inr = coproduct.coidl
        inl = coproduct.coidr

class closed monoidal category
    public
        apply
        curry
        uncurry
    ensures
        curry apply = id
        curry . uncurry = id
        uncurry . curry = id

class braided closed monoidal category
    public
        flip f = curry (uncurry f . braid)

class coclosed comonoidal category
    public
        coapply
        cocurry
        councurry
    ensures
        cocurry coapply = id
        cocurry . councurry = id
        uncocurry . cocurry = id

class closed pre-cartesian category
    public
        const = curry fst

class ccc = closed cartesian category
class co-ccc = coclosed cocartesian category

class arrow : ccc
    public
        pure
    with
        fst = pure K.fst
        snd = pure K.snd

data (a,b)
data Left a
data Right b

data Kata : pre-cocartesian arrow
    with
        id x = x
        (.) f g x = f (g x)
        apply (f,x) = f x
        curry f x y = f (x,y)
        uncurry f (x,y) = f x y
        inl = Left
        inr = Right
        diag x = (x,x)
        codiag (Left x) = x
        codiag (Right x) = x
        (&&&) f g x = (f x, g x)
        (|||) f g (Left x) = f x
        (|||) f g (Right x) = g x
    unifies 
        pure = id

class recursive endofunctor : optimized endofunctor
    public
        in
        out
    with
        cata f = c where c = f . map c . out
        ana g = a where a = in . map c . g
        hylo f g = h where h = f . map h . g
        mendler-cata f = c where c = f c . out
        mendler-ana f = a where a = in . f c
    ensures
        forall f. cata f . in = f . map (cata f)
        cata in = id
        forall g. out . ana g = map (ana g) . g
        ana out = id 

class equirecursive endofunctor : recursive endofunctor
    alias
        id = in = out

data In a
class isorecursive endofunctor : simple recursive functor
    public
        in = In
        out (In x) = x 
       
open Kata

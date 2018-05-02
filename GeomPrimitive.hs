data Point3D a = Point3D a a a deriving Show 
data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a) deriving Show

instance Functor Point3D where
    fmap func (Point3D x y z) = Point3D (func x) (func y) (func z)  

instance Functor GeomPrimitive where
    fmap funk gp = case gp of
                                     (Point x) -> Point (fmap funk x)
                                     (LineSegment x y) -> LineSegment (fmap funk x) (fmap funk y)
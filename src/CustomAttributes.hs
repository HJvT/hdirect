--
-- (c) 2001, sof
--
module CustomAttributes where

import BasicTypes ( Name )
{- This module serves to collect together and document the attribute
   extensions that HDirect makes to (DCE) IDL. At the moment, mostly
   the former!
-}
sequenceAttr   :: Name
sequenceAttr   = "sequence"

terminatorAttr :: Name
terminatorAttr = "terminator"

ignoreAttr     :: Name
ignoreAttr     = "ignore"

newtypeAttr    :: Name
newtypeAttr    = "hs_newtype"

pureAttr       :: Name
pureAttr       = "pure"

flagAttr       :: Name
flagAttr       = "hs_flag"

finaliserAttr  :: Name
finaliserAttr  = "finaliser"

abstractAttr   :: Name
abstractAttr   = "abstract"

foreignAttr    :: Name
foreignAttr    = "foreign"

tyArgAttr      :: Name
tyArgAttr      = "hs_tyarg"

ignoreResAttr  :: Name
ignoreResAttr  = "hs_ignore_result"

noFreeAttr     :: Name
noFreeAttr     = "nofree"

freeAttr       :: Name
freeAttr       = "free"

freeMethodAttr :: Name
freeMethodAttr = "free_method"

errorHandlerAttr :: Name
errorHandlerAttr = "error_handler"

cconvAttr      :: Name
cconvAttr      = "cconv"

derivingAttr   :: Name
derivingAttr   = "deriving"

tyArgsAttr     :: Name
tyArgsAttr     = "ty_args"

ctypeAttr      :: Name
ctypeAttr      = "ctype"

hsNameAttr     :: Name
hsNameAttr     = "hs_name"

tyParamsAttr   :: Name
tyParamsAttr   = "ty_params"

dllNameAttr    :: Name
dllNameAttr    = "dllname"

jniImplementsAttr,jniClassAttr :: Name
jniImplementsAttr = "jni_implements"
jniClassAttr      = "jni_class"

jniGetFieldAttr, jniSetFieldAttr :: Name
jniGetFieldAttr   = "jni_get_field"
jniSetFieldAttr   = "jni_set_field"

jniStaticAttr, jniIfaceTyAttr :: Name
jniStaticAttr     = "jni_static"
jniIfaceTyAttr    = "jni_iface_ty"

jniFinalAttr, jniCtorAttr :: Name
jniFinalAttr      = "jni_final"
jniCtorAttr       = "jni_ctor"

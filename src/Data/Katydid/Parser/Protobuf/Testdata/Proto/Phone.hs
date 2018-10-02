{- This file was auto-generated from phone.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies,
  UndecidableInstances, GeneralizedNewtypeDeriving,
  MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
  PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports#-}
module Data.Katydid.Parser.Protobuf.Testdata.Proto.Phone (PhoneNumber(..)) where
import qualified Data.ProtoLens.Reexport.Lens.Labels.Prism
       as Lens.Labels.Prism
import qualified Data.ProtoLens.Reexport.Prelude as Prelude
import qualified Data.ProtoLens.Reexport.Data.Int as Data.Int
import qualified Data.ProtoLens.Reexport.Data.Word as Data.Word
import qualified Data.ProtoLens.Reexport.Data.ProtoLens
       as Data.ProtoLens
import qualified
       Data.ProtoLens.Reexport.Data.ProtoLens.Message.Enum
       as Data.ProtoLens.Message.Enum
import qualified
       Data.ProtoLens.Reexport.Data.ProtoLens.Service.Types
       as Data.ProtoLens.Service.Types
import qualified Data.ProtoLens.Reexport.Lens.Family2
       as Lens.Family2
import qualified Data.ProtoLens.Reexport.Lens.Family2.Unchecked
       as Lens.Family2.Unchecked
import qualified Data.ProtoLens.Reexport.Data.Default.Class
       as Data.Default.Class
import qualified Data.ProtoLens.Reexport.Data.Text as Data.Text
import qualified Data.ProtoLens.Reexport.Data.Map as Data.Map
import qualified Data.ProtoLens.Reexport.Data.ByteString
       as Data.ByteString
import qualified Data.ProtoLens.Reexport.Data.ByteString.Char8
       as Data.ByteString.Char8
import qualified Data.ProtoLens.Reexport.Lens.Labels as Lens.Labels
import qualified Data.ProtoLens.Reexport.Text.Read as Text.Read

{- | Fields :

    * 'Proto.Phone_Fields.number' @:: Lens' PhoneNumber Data.Text.Text@
    * 'Proto.Phone_Fields.maybe'number' @:: Lens' PhoneNumber (Prelude.Maybe Data.Text.Text)@
 -}
data PhoneNumber = PhoneNumber{_PhoneNumber'number ::
                               !(Prelude.Maybe Data.Text.Text),
                               _PhoneNumber'_unknownFields :: !Data.ProtoLens.FieldSet}
                     deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance (Lens.Labels.HasLens' f PhoneNumber x a, a ~ b) =>
         Lens.Labels.HasLens f PhoneNumber PhoneNumber x a b
         where
        lensOf = Lens.Labels.lensOf'
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f PhoneNumber "number" (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PhoneNumber'number
                 (\ x__ y__ -> x__{_PhoneNumber'number = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f PhoneNumber "maybe'number"
           (Prelude.Maybe Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PhoneNumber'number
                 (\ x__ y__ -> x__{_PhoneNumber'number = y__}))
              Prelude.id
instance Data.Default.Class.Default PhoneNumber where
        def
          = PhoneNumber{_PhoneNumber'number = Prelude.Nothing,
                        _PhoneNumber'_unknownFields = ([])}
instance Data.ProtoLens.Message PhoneNumber where
        messageName _ = Data.Text.pack "phone.PhoneNumber"
        fieldsByTag
          = let number__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "Number"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'number")))
                      :: Data.ProtoLens.FieldDescriptor PhoneNumber
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, number__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _PhoneNumber'_unknownFields
              (\ x__ y__ -> x__{_PhoneNumber'_unknownFields = y__})

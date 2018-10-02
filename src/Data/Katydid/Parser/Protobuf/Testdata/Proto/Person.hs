{- This file was auto-generated from person.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies,
  UndecidableInstances, GeneralizedNewtypeDeriving,
  MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
  PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports#-}
module Data.Katydid.Parser.Protobuf.Testdata.Proto.Person (Person(..), Person'PersonAddress(..)) where
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
import qualified Data.Katydid.Parser.Protobuf.Testdata.Proto.Phone as Proto.Phone

{- | Fields :

    * 'Proto.Person_Fields.name' @:: Lens' Person Data.Text.Text@
    * 'Proto.Person_Fields.maybe'name' @:: Lens' Person (Prelude.Maybe Data.Text.Text)@
    * 'Proto.Person_Fields.address' @:: Lens' Person Person'PersonAddress@
    * 'Proto.Person_Fields.maybe'address' @:: Lens' Person (Prelude.Maybe Person'PersonAddress)@
    * 'Proto.Person_Fields.phoneNumbers' @:: Lens' Person [Proto.Phone.PhoneNumber]@
 -}
data Person = Person{_Person'name ::
                     !(Prelude.Maybe Data.Text.Text),
                     _Person'address :: !(Prelude.Maybe Person'PersonAddress),
                     _Person'phoneNumbers :: ![Proto.Phone.PhoneNumber],
                     _Person'_unknownFields :: !Data.ProtoLens.FieldSet}
                deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance (Lens.Labels.HasLens' f Person x a, a ~ b) =>
         Lens.Labels.HasLens f Person Person x a b
         where
        lensOf = Lens.Labels.lensOf'
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f Person "name" (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Person'name
                 (\ x__ y__ -> x__{_Person'name = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f Person "maybe'name"
           (Prelude.Maybe Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Person'name
                 (\ x__ y__ -> x__{_Person'name = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f Person "address" (Person'PersonAddress)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Person'address
                 (\ x__ y__ -> x__{_Person'address = y__}))
              (Data.ProtoLens.maybeLens Data.Default.Class.def)
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f Person "maybe'address"
           (Prelude.Maybe Person'PersonAddress)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Person'address
                 (\ x__ y__ -> x__{_Person'address = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f Person "phoneNumbers"
           ([Proto.Phone.PhoneNumber])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Person'phoneNumbers
                 (\ x__ y__ -> x__{_Person'phoneNumbers = y__}))
              Prelude.id
instance Data.Default.Class.Default Person where
        def
          = Person{_Person'name = Prelude.Nothing,
                   _Person'address = Prelude.Nothing, _Person'phoneNumbers = [],
                   _Person'_unknownFields = ([])}
instance Data.ProtoLens.Message Person where
        messageName _ = Data.Text.pack "person.Person"
        fieldsByTag
          = let name__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "Name"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'name")))
                      :: Data.ProtoLens.FieldDescriptor Person
                address__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "Address"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor Person'PersonAddress)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'address")))
                      :: Data.ProtoLens.FieldDescriptor Person
                phoneNumbers__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "PhoneNumbers"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor Proto.Phone.PhoneNumber)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "phoneNumbers")))
                      :: Data.ProtoLens.FieldDescriptor Person
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, name__field_descriptor),
                 (Data.ProtoLens.Tag 2, address__field_descriptor),
                 (Data.ProtoLens.Tag 3, phoneNumbers__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _Person'_unknownFields
              (\ x__ y__ -> x__{_Person'_unknownFields = y__})
{- | Fields :

    * 'Proto.Person_Fields.street' @:: Lens' Person'PersonAddress Data.Text.Text@
    * 'Proto.Person_Fields.maybe'street' @:: Lens' Person'PersonAddress (Prelude.Maybe Data.Text.Text)@
    * 'Proto.Person_Fields.number' @:: Lens' Person'PersonAddress Data.Word.Word64@
    * 'Proto.Person_Fields.maybe'number' @:: Lens' Person'PersonAddress (Prelude.Maybe Data.Word.Word64)@
 -}
data Person'PersonAddress = Person'PersonAddress{_Person'PersonAddress'street
                                                 :: !(Prelude.Maybe Data.Text.Text),
                                                 _Person'PersonAddress'number ::
                                                 !(Prelude.Maybe Data.Word.Word64),
                                                 _Person'PersonAddress'_unknownFields ::
                                                 !Data.ProtoLens.FieldSet}
                              deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance (Lens.Labels.HasLens' f Person'PersonAddress x a,
          a ~ b) =>
         Lens.Labels.HasLens f Person'PersonAddress Person'PersonAddress x a
           b
         where
        lensOf = Lens.Labels.lensOf'
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f Person'PersonAddress "street"
           (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Person'PersonAddress'street
                 (\ x__ y__ -> x__{_Person'PersonAddress'street = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f Person'PersonAddress "maybe'street"
           (Prelude.Maybe Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Person'PersonAddress'street
                 (\ x__ y__ -> x__{_Person'PersonAddress'street = y__}))
              Prelude.id
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f Person'PersonAddress "number"
           (Data.Word.Word64)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Person'PersonAddress'number
                 (\ x__ y__ -> x__{_Person'PersonAddress'number = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Prelude.Functor f =>
         Lens.Labels.HasLens' f Person'PersonAddress "maybe'number"
           (Prelude.Maybe Data.Word.Word64)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Person'PersonAddress'number
                 (\ x__ y__ -> x__{_Person'PersonAddress'number = y__}))
              Prelude.id
instance Data.Default.Class.Default Person'PersonAddress where
        def
          = Person'PersonAddress{_Person'PersonAddress'street =
                                   Prelude.Nothing,
                                 _Person'PersonAddress'number = Prelude.Nothing,
                                 _Person'PersonAddress'_unknownFields = ([])}
instance Data.ProtoLens.Message Person'PersonAddress where
        messageName _ = Data.Text.pack "person.Person.PersonAddress"
        fieldsByTag
          = let street__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "Street"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'street")))
                      :: Data.ProtoLens.FieldDescriptor Person'PersonAddress
                number__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "Number"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'number")))
                      :: Data.ProtoLens.FieldDescriptor Person'PersonAddress
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, street__field_descriptor),
                 (Data.ProtoLens.Tag 2, number__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _Person'PersonAddress'_unknownFields
              (\ x__ y__ -> x__{_Person'PersonAddress'_unknownFields = y__})

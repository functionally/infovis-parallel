# -*- coding: utf-8 -*-
# Generated by the protocol buffer compiler.  DO NOT EDIT!
# source: infovis.proto3

import sys
_b=sys.version_info[0]<3 and (lambda x:x) or (lambda x:x.encode('latin1'))
from google.protobuf import descriptor as _descriptor
from google.protobuf import message as _message
from google.protobuf import reflection as _reflection
from google.protobuf import symbol_database as _symbol_database
# @@protoc_insertion_point(imports)

_sym_db = _symbol_database.Default()




DESCRIPTOR = _descriptor.FileDescriptor(
  name='infovis.proto3',
  package='Infovis',
  syntax='proto3',
  serialized_options=_b('\252\002\020Infovis.Protobuf'),
  serialized_pb=_b('\n\x0einfovis.proto3\x12\x07Infovis\"\xb2\x01\n\x08Geometry\x12\x0c\n\x04\x66ram\x18\x01 \x01(\x05\x12\x0c\n\x04iden\x18\x02 \x01(\x03\x12\x0c\n\x04type\x18\x03 \x01(\x05\x12\x0c\n\x04mask\x18\x04 \x01(\x05\x12\x0c\n\x04\x63nts\x18\x05 \x03(\x05\x12\x0c\n\x04posx\x18\x06 \x03(\x01\x12\x0c\n\x04posy\x18\x07 \x03(\x01\x12\x0c\n\x04posz\x18\x08 \x03(\x01\x12\x0c\n\x04size\x18\t \x01(\x01\x12\x0c\n\x04\x63olr\x18\n \x01(\x07\x12\x0c\n\x04text\x18\x0b \x01(\t\x12\x0c\n\x04glyp\x18\x0c \x01(\x05\"l\n\x08Location\x12\x0c\n\x04posx\x18\x01 \x01(\x01\x12\x0c\n\x04posy\x18\x02 \x01(\x01\x12\x0c\n\x04posz\x18\x03 \x01(\x01\x12\x0c\n\x04rotw\x18\x04 \x01(\x01\x12\x0c\n\x04rotx\x18\x05 \x01(\x01\x12\x0c\n\x04roty\x18\x06 \x01(\x01\x12\x0c\n\x04rotz\x18\x07 \x01(\x01\"\xd8\x01\n\x07Request\x12\x0c\n\x04show\x18\x01 \x01(\x05\x12\x0f\n\x07message\x18\x02 \x01(\t\x12\r\n\x05reset\x18\x03 \x01(\x08\x12!\n\x06upsert\x18\x04 \x03(\x0b\x32\x11.Infovis.Geometry\x12\x0e\n\x06\x64\x65lete\x18\x05 \x03(\x03\x12\"\n\x07viewloc\x18\x06 \x01(\x0b\x32\x11.Infovis.Location\x12\"\n\x07toolloc\x18\x07 \x01(\x0b\x32\x11.Infovis.Location\x12$\n\toffsetloc\x18\x08 \x01(\x0b\x32\x11.Infovis.Location\"\xa0\x02\n\x08Response\x12\r\n\x05shown\x18\x01 \x01(\x05\x12\x0f\n\x07message\x18\x02 \x01(\t\x12\r\n\x05hover\x18\x03 \x03(\x03\x12\x0f\n\x07unhover\x18\x04 \x03(\x03\x12\x0e\n\x06select\x18\x05 \x03(\x03\x12\x10\n\x08\x64\x65select\x18\x06 \x03(\x03\x12\"\n\x07viewloc\x18\x07 \x01(\x0b\x32\x11.Infovis.Location\x12\"\n\x07toolloc\x18\x08 \x01(\x0b\x32\x11.Infovis.Location\x12$\n\toffsetloc\x18\t \x01(\x0b\x32\x11.Infovis.Location\x12\x11\n\tdepressed\x18\n \x01(\x07\x12\x0f\n\x07pressed\x18\x0b \x01(\x07\x12\x10\n\x08released\x18\x0c \x01(\x07\x12\x0e\n\x06\x61nalog\x18\r \x03(\x01\x42\x13\xaa\x02\x10Infovis.Protobufb\x06proto3')
)




_GEOMETRY = _descriptor.Descriptor(
  name='Geometry',
  full_name='Infovis.Geometry',
  filename=None,
  file=DESCRIPTOR,
  containing_type=None,
  fields=[
    _descriptor.FieldDescriptor(
      name='fram', full_name='Infovis.Geometry.fram', index=0,
      number=1, type=5, cpp_type=1, label=1,
      has_default_value=False, default_value=0,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='iden', full_name='Infovis.Geometry.iden', index=1,
      number=2, type=3, cpp_type=2, label=1,
      has_default_value=False, default_value=0,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='type', full_name='Infovis.Geometry.type', index=2,
      number=3, type=5, cpp_type=1, label=1,
      has_default_value=False, default_value=0,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='mask', full_name='Infovis.Geometry.mask', index=3,
      number=4, type=5, cpp_type=1, label=1,
      has_default_value=False, default_value=0,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='cnts', full_name='Infovis.Geometry.cnts', index=4,
      number=5, type=5, cpp_type=1, label=3,
      has_default_value=False, default_value=[],
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='posx', full_name='Infovis.Geometry.posx', index=5,
      number=6, type=1, cpp_type=5, label=3,
      has_default_value=False, default_value=[],
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='posy', full_name='Infovis.Geometry.posy', index=6,
      number=7, type=1, cpp_type=5, label=3,
      has_default_value=False, default_value=[],
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='posz', full_name='Infovis.Geometry.posz', index=7,
      number=8, type=1, cpp_type=5, label=3,
      has_default_value=False, default_value=[],
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='size', full_name='Infovis.Geometry.size', index=8,
      number=9, type=1, cpp_type=5, label=1,
      has_default_value=False, default_value=float(0),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='colr', full_name='Infovis.Geometry.colr', index=9,
      number=10, type=7, cpp_type=3, label=1,
      has_default_value=False, default_value=0,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='text', full_name='Infovis.Geometry.text', index=10,
      number=11, type=9, cpp_type=9, label=1,
      has_default_value=False, default_value=_b("").decode('utf-8'),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='glyp', full_name='Infovis.Geometry.glyp', index=11,
      number=12, type=5, cpp_type=1, label=1,
      has_default_value=False, default_value=0,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
  ],
  extensions=[
  ],
  nested_types=[],
  enum_types=[
  ],
  serialized_options=None,
  is_extendable=False,
  syntax='proto3',
  extension_ranges=[],
  oneofs=[
  ],
  serialized_start=28,
  serialized_end=206,
)


_LOCATION = _descriptor.Descriptor(
  name='Location',
  full_name='Infovis.Location',
  filename=None,
  file=DESCRIPTOR,
  containing_type=None,
  fields=[
    _descriptor.FieldDescriptor(
      name='posx', full_name='Infovis.Location.posx', index=0,
      number=1, type=1, cpp_type=5, label=1,
      has_default_value=False, default_value=float(0),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='posy', full_name='Infovis.Location.posy', index=1,
      number=2, type=1, cpp_type=5, label=1,
      has_default_value=False, default_value=float(0),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='posz', full_name='Infovis.Location.posz', index=2,
      number=3, type=1, cpp_type=5, label=1,
      has_default_value=False, default_value=float(0),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='rotw', full_name='Infovis.Location.rotw', index=3,
      number=4, type=1, cpp_type=5, label=1,
      has_default_value=False, default_value=float(0),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='rotx', full_name='Infovis.Location.rotx', index=4,
      number=5, type=1, cpp_type=5, label=1,
      has_default_value=False, default_value=float(0),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='roty', full_name='Infovis.Location.roty', index=5,
      number=6, type=1, cpp_type=5, label=1,
      has_default_value=False, default_value=float(0),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='rotz', full_name='Infovis.Location.rotz', index=6,
      number=7, type=1, cpp_type=5, label=1,
      has_default_value=False, default_value=float(0),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
  ],
  extensions=[
  ],
  nested_types=[],
  enum_types=[
  ],
  serialized_options=None,
  is_extendable=False,
  syntax='proto3',
  extension_ranges=[],
  oneofs=[
  ],
  serialized_start=208,
  serialized_end=316,
)


_REQUEST = _descriptor.Descriptor(
  name='Request',
  full_name='Infovis.Request',
  filename=None,
  file=DESCRIPTOR,
  containing_type=None,
  fields=[
    _descriptor.FieldDescriptor(
      name='show', full_name='Infovis.Request.show', index=0,
      number=1, type=5, cpp_type=1, label=1,
      has_default_value=False, default_value=0,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='message', full_name='Infovis.Request.message', index=1,
      number=2, type=9, cpp_type=9, label=1,
      has_default_value=False, default_value=_b("").decode('utf-8'),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='reset', full_name='Infovis.Request.reset', index=2,
      number=3, type=8, cpp_type=7, label=1,
      has_default_value=False, default_value=False,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='upsert', full_name='Infovis.Request.upsert', index=3,
      number=4, type=11, cpp_type=10, label=3,
      has_default_value=False, default_value=[],
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='delete', full_name='Infovis.Request.delete', index=4,
      number=5, type=3, cpp_type=2, label=3,
      has_default_value=False, default_value=[],
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='viewloc', full_name='Infovis.Request.viewloc', index=5,
      number=6, type=11, cpp_type=10, label=1,
      has_default_value=False, default_value=None,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='toolloc', full_name='Infovis.Request.toolloc', index=6,
      number=7, type=11, cpp_type=10, label=1,
      has_default_value=False, default_value=None,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='offsetloc', full_name='Infovis.Request.offsetloc', index=7,
      number=8, type=11, cpp_type=10, label=1,
      has_default_value=False, default_value=None,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
  ],
  extensions=[
  ],
  nested_types=[],
  enum_types=[
  ],
  serialized_options=None,
  is_extendable=False,
  syntax='proto3',
  extension_ranges=[],
  oneofs=[
  ],
  serialized_start=319,
  serialized_end=535,
)


_RESPONSE = _descriptor.Descriptor(
  name='Response',
  full_name='Infovis.Response',
  filename=None,
  file=DESCRIPTOR,
  containing_type=None,
  fields=[
    _descriptor.FieldDescriptor(
      name='shown', full_name='Infovis.Response.shown', index=0,
      number=1, type=5, cpp_type=1, label=1,
      has_default_value=False, default_value=0,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='message', full_name='Infovis.Response.message', index=1,
      number=2, type=9, cpp_type=9, label=1,
      has_default_value=False, default_value=_b("").decode('utf-8'),
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='hover', full_name='Infovis.Response.hover', index=2,
      number=3, type=3, cpp_type=2, label=3,
      has_default_value=False, default_value=[],
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='unhover', full_name='Infovis.Response.unhover', index=3,
      number=4, type=3, cpp_type=2, label=3,
      has_default_value=False, default_value=[],
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='select', full_name='Infovis.Response.select', index=4,
      number=5, type=3, cpp_type=2, label=3,
      has_default_value=False, default_value=[],
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='deselect', full_name='Infovis.Response.deselect', index=5,
      number=6, type=3, cpp_type=2, label=3,
      has_default_value=False, default_value=[],
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='viewloc', full_name='Infovis.Response.viewloc', index=6,
      number=7, type=11, cpp_type=10, label=1,
      has_default_value=False, default_value=None,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='toolloc', full_name='Infovis.Response.toolloc', index=7,
      number=8, type=11, cpp_type=10, label=1,
      has_default_value=False, default_value=None,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='offsetloc', full_name='Infovis.Response.offsetloc', index=8,
      number=9, type=11, cpp_type=10, label=1,
      has_default_value=False, default_value=None,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='depressed', full_name='Infovis.Response.depressed', index=9,
      number=10, type=7, cpp_type=3, label=1,
      has_default_value=False, default_value=0,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='pressed', full_name='Infovis.Response.pressed', index=10,
      number=11, type=7, cpp_type=3, label=1,
      has_default_value=False, default_value=0,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='released', full_name='Infovis.Response.released', index=11,
      number=12, type=7, cpp_type=3, label=1,
      has_default_value=False, default_value=0,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
    _descriptor.FieldDescriptor(
      name='analog', full_name='Infovis.Response.analog', index=12,
      number=13, type=1, cpp_type=5, label=3,
      has_default_value=False, default_value=[],
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      serialized_options=None, file=DESCRIPTOR),
  ],
  extensions=[
  ],
  nested_types=[],
  enum_types=[
  ],
  serialized_options=None,
  is_extendable=False,
  syntax='proto3',
  extension_ranges=[],
  oneofs=[
  ],
  serialized_start=538,
  serialized_end=826,
)

_REQUEST.fields_by_name['upsert'].message_type = _GEOMETRY
_REQUEST.fields_by_name['viewloc'].message_type = _LOCATION
_REQUEST.fields_by_name['toolloc'].message_type = _LOCATION
_REQUEST.fields_by_name['offsetloc'].message_type = _LOCATION
_RESPONSE.fields_by_name['viewloc'].message_type = _LOCATION
_RESPONSE.fields_by_name['toolloc'].message_type = _LOCATION
_RESPONSE.fields_by_name['offsetloc'].message_type = _LOCATION
DESCRIPTOR.message_types_by_name['Geometry'] = _GEOMETRY
DESCRIPTOR.message_types_by_name['Location'] = _LOCATION
DESCRIPTOR.message_types_by_name['Request'] = _REQUEST
DESCRIPTOR.message_types_by_name['Response'] = _RESPONSE
_sym_db.RegisterFileDescriptor(DESCRIPTOR)

Geometry = _reflection.GeneratedProtocolMessageType('Geometry', (_message.Message,), dict(
  DESCRIPTOR = _GEOMETRY,
  __module__ = 'infovis.proto3_pb2'
  # @@protoc_insertion_point(class_scope:Infovis.Geometry)
  ))
_sym_db.RegisterMessage(Geometry)

Location = _reflection.GeneratedProtocolMessageType('Location', (_message.Message,), dict(
  DESCRIPTOR = _LOCATION,
  __module__ = 'infovis.proto3_pb2'
  # @@protoc_insertion_point(class_scope:Infovis.Location)
  ))
_sym_db.RegisterMessage(Location)

Request = _reflection.GeneratedProtocolMessageType('Request', (_message.Message,), dict(
  DESCRIPTOR = _REQUEST,
  __module__ = 'infovis.proto3_pb2'
  # @@protoc_insertion_point(class_scope:Infovis.Request)
  ))
_sym_db.RegisterMessage(Request)

Response = _reflection.GeneratedProtocolMessageType('Response', (_message.Message,), dict(
  DESCRIPTOR = _RESPONSE,
  __module__ = 'infovis.proto3_pb2'
  # @@protoc_insertion_point(class_scope:Infovis.Response)
  ))
_sym_db.RegisterMessage(Response)


DESCRIPTOR._options = None
# @@protoc_insertion_point(module_scope)

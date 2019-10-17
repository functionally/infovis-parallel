/**
 * @fileoverview
 * @enhanceable
 * @suppress {messageConventions} JS Compiler reports an error if a variable or
 *     field starts with 'MSG_' and isn't a translatable message.
 * @public
 */
// GENERATED CODE -- DO NOT EDIT!

goog.provide('proto.Infovis.Location');

goog.require('jspb.BinaryReader');
goog.require('jspb.BinaryWriter');
goog.require('jspb.Message');

/**
 * Generated by JsPbCodeGenerator.
 * @param {Array=} opt_data Optional initial data array, typically from a
 * server response, or constructed directly in Javascript. The array is used
 * in place and becomes part of the constructed object. It is not cloned.
 * If no data is provided, the constructed object will be empty, but still
 * valid.
 * @extends {jspb.Message}
 * @constructor
 */
proto.Infovis.Location = function(opt_data) {
  jspb.Message.initialize(this, opt_data, 0, -1, null, null);
};
goog.inherits(proto.Infovis.Location, jspb.Message);
if (goog.DEBUG && !COMPILED) {
  /**
   * @public
   * @override
   */
  proto.Infovis.Location.displayName = 'proto.Infovis.Location';
}



if (jspb.Message.GENERATE_TO_OBJECT) {
/**
 * Creates an object representation of this proto suitable for use in Soy templates.
 * Field names that are reserved in JavaScript and will be renamed to pb_name.
 * To access a reserved field use, foo.pb_<name>, eg, foo.pb_default.
 * For the list of reserved names please see:
 *     com.google.apps.jspb.JsClassTemplate.JS_RESERVED_WORDS.
 * @param {boolean=} opt_includeInstance Whether to include the JSPB instance
 *     for transitional soy proto support: http://goto/soy-param-migration
 * @return {!Object}
 */
proto.Infovis.Location.prototype.toObject = function(opt_includeInstance) {
  return proto.Infovis.Location.toObject(opt_includeInstance, this);
};


/**
 * Static version of the {@see toObject} method.
 * @param {boolean|undefined} includeInstance Whether to include the JSPB
 *     instance for transitional soy proto support:
 *     http://goto/soy-param-migration
 * @param {!proto.Infovis.Location} msg The msg instance to transform.
 * @return {!Object}
 * @suppress {unusedLocalVariables} f is only used for nested messages
 */
proto.Infovis.Location.toObject = function(includeInstance, msg) {
  var f, obj = {
    posx: +jspb.Message.getFieldWithDefault(msg, 1, 0.0),
    posy: +jspb.Message.getFieldWithDefault(msg, 2, 0.0),
    posz: +jspb.Message.getFieldWithDefault(msg, 3, 0.0),
    rotw: +jspb.Message.getFieldWithDefault(msg, 4, 0.0),
    rotx: +jspb.Message.getFieldWithDefault(msg, 5, 0.0),
    roty: +jspb.Message.getFieldWithDefault(msg, 6, 0.0),
    rotz: +jspb.Message.getFieldWithDefault(msg, 7, 0.0)
  };

  if (includeInstance) {
    obj.$jspbMessageInstance = msg;
  }
  return obj;
};
}


/**
 * Deserializes binary data (in protobuf wire format).
 * @param {jspb.ByteSource} bytes The bytes to deserialize.
 * @return {!proto.Infovis.Location}
 */
proto.Infovis.Location.deserializeBinary = function(bytes) {
  var reader = new jspb.BinaryReader(bytes);
  var msg = new proto.Infovis.Location;
  return proto.Infovis.Location.deserializeBinaryFromReader(msg, reader);
};


/**
 * Deserializes binary data (in protobuf wire format) from the
 * given reader into the given message object.
 * @param {!proto.Infovis.Location} msg The message object to deserialize into.
 * @param {!jspb.BinaryReader} reader The BinaryReader to use.
 * @return {!proto.Infovis.Location}
 */
proto.Infovis.Location.deserializeBinaryFromReader = function(msg, reader) {
  while (reader.nextField()) {
    if (reader.isEndGroup()) {
      break;
    }
    var field = reader.getFieldNumber();
    switch (field) {
    case 1:
      var value = /** @type {number} */ (reader.readDouble());
      msg.setPosx(value);
      break;
    case 2:
      var value = /** @type {number} */ (reader.readDouble());
      msg.setPosy(value);
      break;
    case 3:
      var value = /** @type {number} */ (reader.readDouble());
      msg.setPosz(value);
      break;
    case 4:
      var value = /** @type {number} */ (reader.readDouble());
      msg.setRotw(value);
      break;
    case 5:
      var value = /** @type {number} */ (reader.readDouble());
      msg.setRotx(value);
      break;
    case 6:
      var value = /** @type {number} */ (reader.readDouble());
      msg.setRoty(value);
      break;
    case 7:
      var value = /** @type {number} */ (reader.readDouble());
      msg.setRotz(value);
      break;
    default:
      reader.skipField();
      break;
    }
  }
  return msg;
};


/**
 * Serializes the message to binary data (in protobuf wire format).
 * @return {!Uint8Array}
 */
proto.Infovis.Location.prototype.serializeBinary = function() {
  var writer = new jspb.BinaryWriter();
  proto.Infovis.Location.serializeBinaryToWriter(this, writer);
  return writer.getResultBuffer();
};


/**
 * Serializes the given message to binary data (in protobuf wire
 * format), writing to the given BinaryWriter.
 * @param {!proto.Infovis.Location} message
 * @param {!jspb.BinaryWriter} writer
 * @suppress {unusedLocalVariables} f is only used for nested messages
 */
proto.Infovis.Location.serializeBinaryToWriter = function(message, writer) {
  var f = undefined;
  f = message.getPosx();
  if (f !== 0.0) {
    writer.writeDouble(
      1,
      f
    );
  }
  f = message.getPosy();
  if (f !== 0.0) {
    writer.writeDouble(
      2,
      f
    );
  }
  f = message.getPosz();
  if (f !== 0.0) {
    writer.writeDouble(
      3,
      f
    );
  }
  f = message.getRotw();
  if (f !== 0.0) {
    writer.writeDouble(
      4,
      f
    );
  }
  f = message.getRotx();
  if (f !== 0.0) {
    writer.writeDouble(
      5,
      f
    );
  }
  f = message.getRoty();
  if (f !== 0.0) {
    writer.writeDouble(
      6,
      f
    );
  }
  f = message.getRotz();
  if (f !== 0.0) {
    writer.writeDouble(
      7,
      f
    );
  }
};


/**
 * optional double posx = 1;
 * @return {number}
 */
proto.Infovis.Location.prototype.getPosx = function() {
  return /** @type {number} */ (+jspb.Message.getFieldWithDefault(this, 1, 0.0));
};


/** @param {number} value */
proto.Infovis.Location.prototype.setPosx = function(value) {
  jspb.Message.setProto3FloatField(this, 1, value);
};


/**
 * optional double posy = 2;
 * @return {number}
 */
proto.Infovis.Location.prototype.getPosy = function() {
  return /** @type {number} */ (+jspb.Message.getFieldWithDefault(this, 2, 0.0));
};


/** @param {number} value */
proto.Infovis.Location.prototype.setPosy = function(value) {
  jspb.Message.setProto3FloatField(this, 2, value);
};


/**
 * optional double posz = 3;
 * @return {number}
 */
proto.Infovis.Location.prototype.getPosz = function() {
  return /** @type {number} */ (+jspb.Message.getFieldWithDefault(this, 3, 0.0));
};


/** @param {number} value */
proto.Infovis.Location.prototype.setPosz = function(value) {
  jspb.Message.setProto3FloatField(this, 3, value);
};


/**
 * optional double rotw = 4;
 * @return {number}
 */
proto.Infovis.Location.prototype.getRotw = function() {
  return /** @type {number} */ (+jspb.Message.getFieldWithDefault(this, 4, 0.0));
};


/** @param {number} value */
proto.Infovis.Location.prototype.setRotw = function(value) {
  jspb.Message.setProto3FloatField(this, 4, value);
};


/**
 * optional double rotx = 5;
 * @return {number}
 */
proto.Infovis.Location.prototype.getRotx = function() {
  return /** @type {number} */ (+jspb.Message.getFieldWithDefault(this, 5, 0.0));
};


/** @param {number} value */
proto.Infovis.Location.prototype.setRotx = function(value) {
  jspb.Message.setProto3FloatField(this, 5, value);
};


/**
 * optional double roty = 6;
 * @return {number}
 */
proto.Infovis.Location.prototype.getRoty = function() {
  return /** @type {number} */ (+jspb.Message.getFieldWithDefault(this, 6, 0.0));
};


/** @param {number} value */
proto.Infovis.Location.prototype.setRoty = function(value) {
  jspb.Message.setProto3FloatField(this, 6, value);
};


/**
 * optional double rotz = 7;
 * @return {number}
 */
proto.Infovis.Location.prototype.getRotz = function() {
  return /** @type {number} */ (+jspb.Message.getFieldWithDefault(this, 7, 0.0));
};


/** @param {number} value */
proto.Infovis.Location.prototype.setRotz = function(value) {
  jspb.Message.setProto3FloatField(this, 7, value);
};



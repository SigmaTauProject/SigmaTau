// automatically generated by the FlatBuffers compiler, do not modify

/**
 * @const
 * @namespace
 */
var Msg = Msg || {};

/**
 * @const
 * @namespace
 */
Msg.Common = Msg.Common || {};

/**
 * @const
 * @namespace
 */
Msg.Wire = Msg.Wire || {};

/**
 * @enum
 */
Msg.Wire.UpMsgContent = {
  NONE: 0,
  Set: 1,
  Adjust: 2
};

/**
 * @enum
 */
Msg.Wire.DownMsgContent = {
  NONE: 0,
  Set: 1
};

/**
 * @constructor
 */
Msg.Wire.Set = function() {
  /**
   * @type {flatbuffers.ByteBuffer}
   */
  this.bb = null;

  /**
   * @type {number}
   */
  this.bb_pos = 0;
};

/**
 * @param {number} i
 * @param {flatbuffers.ByteBuffer} bb
 * @returns {Msg.Wire.Set}
 */
Msg.Wire.Set.prototype.__init = function(i, bb) {
  this.bb_pos = i;
  this.bb = bb;
  return this;
};

/**
 * @param {flatbuffers.ByteBuffer} bb
 * @param {Msg.Wire.Set=} obj
 * @returns {Msg.Wire.Set}
 */
Msg.Wire.Set.getRootAsSet = function(bb, obj) {
  return (obj || new Msg.Wire.Set).__init(bb.readInt32(bb.position()) + bb.position(), bb);
};

/**
 * @returns {number}
 */
Msg.Wire.Set.prototype.value = function() {
  var offset = this.bb.__offset(this.bb_pos, 4);
  return offset ? this.bb.readInt16(this.bb_pos + offset) : 0;
};

/**
 * @param {flatbuffers.Builder} builder
 */
Msg.Wire.Set.startSet = function(builder) {
  builder.startObject(1);
};

/**
 * @param {flatbuffers.Builder} builder
 * @param {number} value
 */
Msg.Wire.Set.addValue = function(builder, value) {
  builder.addFieldInt16(0, value, 0);
};

/**
 * @param {flatbuffers.Builder} builder
 * @returns {flatbuffers.Offset}
 */
Msg.Wire.Set.endSet = function(builder) {
  var offset = builder.endObject();
  return offset;
};

/**
 * @constructor
 */
Msg.Wire.Adjust = function() {
  /**
   * @type {flatbuffers.ByteBuffer}
   */
  this.bb = null;

  /**
   * @type {number}
   */
  this.bb_pos = 0;
};

/**
 * @param {number} i
 * @param {flatbuffers.ByteBuffer} bb
 * @returns {Msg.Wire.Adjust}
 */
Msg.Wire.Adjust.prototype.__init = function(i, bb) {
  this.bb_pos = i;
  this.bb = bb;
  return this;
};

/**
 * @param {flatbuffers.ByteBuffer} bb
 * @param {Msg.Wire.Adjust=} obj
 * @returns {Msg.Wire.Adjust}
 */
Msg.Wire.Adjust.getRootAsAdjust = function(bb, obj) {
  return (obj || new Msg.Wire.Adjust).__init(bb.readInt32(bb.position()) + bb.position(), bb);
};

/**
 * @returns {number}
 */
Msg.Wire.Adjust.prototype.value = function() {
  var offset = this.bb.__offset(this.bb_pos, 4);
  return offset ? this.bb.readInt16(this.bb_pos + offset) : 0;
};

/**
 * @param {flatbuffers.Builder} builder
 */
Msg.Wire.Adjust.startAdjust = function(builder) {
  builder.startObject(1);
};

/**
 * @param {flatbuffers.Builder} builder
 * @param {number} value
 */
Msg.Wire.Adjust.addValue = function(builder, value) {
  builder.addFieldInt16(0, value, 0);
};

/**
 * @param {flatbuffers.Builder} builder
 * @returns {flatbuffers.Offset}
 */
Msg.Wire.Adjust.endAdjust = function(builder) {
  var offset = builder.endObject();
  return offset;
};

/**
 * @constructor
 */
Msg.Wire.UpMsg = function() {
  /**
   * @type {flatbuffers.ByteBuffer}
   */
  this.bb = null;

  /**
   * @type {number}
   */
  this.bb_pos = 0;
};

/**
 * @param {number} i
 * @param {flatbuffers.ByteBuffer} bb
 * @returns {Msg.Wire.UpMsg}
 */
Msg.Wire.UpMsg.prototype.__init = function(i, bb) {
  this.bb_pos = i;
  this.bb = bb;
  return this;
};

/**
 * @param {flatbuffers.ByteBuffer} bb
 * @param {Msg.Wire.UpMsg=} obj
 * @returns {Msg.Wire.UpMsg}
 */
Msg.Wire.UpMsg.getRootAsUpMsg = function(bb, obj) {
  return (obj || new Msg.Wire.UpMsg).__init(bb.readInt32(bb.position()) + bb.position(), bb);
};

/**
 * @returns {Msg.Wire.UpMsgContent}
 */
Msg.Wire.UpMsg.prototype.contentType = function() {
  var offset = this.bb.__offset(this.bb_pos, 4);
  return offset ? /** @type {Msg.Wire.UpMsgContent} */ (this.bb.readUint8(this.bb_pos + offset)) : Msg.Wire.UpMsgContent.NONE;
};

/**
 * @param {flatbuffers.Table} obj
 * @returns {?flatbuffers.Table}
 */
Msg.Wire.UpMsg.prototype.content = function(obj) {
  var offset = this.bb.__offset(this.bb_pos, 6);
  return offset ? this.bb.__union(obj, this.bb_pos + offset) : null;
};

/**
 * @param {flatbuffers.Builder} builder
 */
Msg.Wire.UpMsg.startUpMsg = function(builder) {
  builder.startObject(2);
};

/**
 * @param {flatbuffers.Builder} builder
 * @param {Msg.Wire.UpMsgContent} contentType
 */
Msg.Wire.UpMsg.addContentType = function(builder, contentType) {
  builder.addFieldInt8(0, contentType, Msg.Wire.UpMsgContent.NONE);
};

/**
 * @param {flatbuffers.Builder} builder
 * @param {flatbuffers.Offset} contentOffset
 */
Msg.Wire.UpMsg.addContent = function(builder, contentOffset) {
  builder.addFieldOffset(1, contentOffset, 0);
};

/**
 * @param {flatbuffers.Builder} builder
 * @returns {flatbuffers.Offset}
 */
Msg.Wire.UpMsg.endUpMsg = function(builder) {
  var offset = builder.endObject();
  return offset;
};

/**
 * @constructor
 */
Msg.Wire.DownpMsg = function() {
  /**
   * @type {flatbuffers.ByteBuffer}
   */
  this.bb = null;

  /**
   * @type {number}
   */
  this.bb_pos = 0;
};

/**
 * @param {number} i
 * @param {flatbuffers.ByteBuffer} bb
 * @returns {Msg.Wire.DownpMsg}
 */
Msg.Wire.DownpMsg.prototype.__init = function(i, bb) {
  this.bb_pos = i;
  this.bb = bb;
  return this;
};

/**
 * @param {flatbuffers.ByteBuffer} bb
 * @param {Msg.Wire.DownpMsg=} obj
 * @returns {Msg.Wire.DownpMsg}
 */
Msg.Wire.DownpMsg.getRootAsDownpMsg = function(bb, obj) {
  return (obj || new Msg.Wire.DownpMsg).__init(bb.readInt32(bb.position()) + bb.position(), bb);
};

/**
 * @returns {Msg.Wire.DownMsgContent}
 */
Msg.Wire.DownpMsg.prototype.contentType = function() {
  var offset = this.bb.__offset(this.bb_pos, 4);
  return offset ? /** @type {Msg.Wire.DownMsgContent} */ (this.bb.readUint8(this.bb_pos + offset)) : Msg.Wire.DownMsgContent.NONE;
};

/**
 * @param {flatbuffers.Table} obj
 * @returns {?flatbuffers.Table}
 */
Msg.Wire.DownpMsg.prototype.content = function(obj) {
  var offset = this.bb.__offset(this.bb_pos, 6);
  return offset ? this.bb.__union(obj, this.bb_pos + offset) : null;
};

/**
 * @param {flatbuffers.Builder} builder
 */
Msg.Wire.DownpMsg.startDownpMsg = function(builder) {
  builder.startObject(2);
};

/**
 * @param {flatbuffers.Builder} builder
 * @param {Msg.Wire.DownMsgContent} contentType
 */
Msg.Wire.DownpMsg.addContentType = function(builder, contentType) {
  builder.addFieldInt8(0, contentType, Msg.Wire.DownMsgContent.NONE);
};

/**
 * @param {flatbuffers.Builder} builder
 * @param {flatbuffers.Offset} contentOffset
 */
Msg.Wire.DownpMsg.addContent = function(builder, contentOffset) {
  builder.addFieldOffset(1, contentOffset, 0);
};

/**
 * @param {flatbuffers.Builder} builder
 * @returns {flatbuffers.Offset}
 */
Msg.Wire.DownpMsg.endDownpMsg = function(builder) {
  var offset = builder.endObject();
  return offset;
};

// Exports for Node.js and RequireJS
this.Msg = Msg;

// see blog post: http://www.hiddentao.com/archives/2013/07/08/generate-overridable-getters-and-setters-in-javascript/
Function.prototype.genProp = function(name, options) {
    // internal member variable name
    var privateName = '__' + name;

    options = options || {};
    options.get = ('undefined' === typeof options.get ? true : options.get );
    options.set = ('undefined' === typeof options.set ? true : options.set );

    // pre-initialise the internal variable?
    if (options.defaultValue) {
      this.prototype[privateName] = options.defaultValue;
    }

    var definePropOptions = {},
      getterName = '__get_' + name,
      setterName = '__set_' + name;

    // generate the getter
    if(true === options.get) {
      this.prototype[getterName] = function() {
        return this[privateName];
      };
    }
    // use custom getter
    else if (options.get) {
      this.prototype[getterName] = options.get;
    }
    // disable getter
    else {
      this.prototype[getterName] = function() {
        throw new Error('Cannot get: ' + name);
      }
    }

    definePropOptions.get = function() {
      return this[getterName].call(this);
    };

    // generate the setter
    if(true === options.set) {
      this.prototype[setterName] = function(val) {
        this[privateName] = val;
      };
    }
    // use custom setter
    else if (options.set) {
      this.prototype[setterName] = options.set;
    }
    // disable setter
    else {
      this.prototype[setterName] = function() {
        throw new Error('Cannot set: ' + name)
      };
    }

    definePropOptions.set = function(val) {
      this[setterName].call(this, val);
    };

    // do it!
    Object.defineProperty(this.prototype, name, definePropOptions);
  };


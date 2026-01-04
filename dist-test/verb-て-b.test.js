var __defProp = Object.defineProperty;
var __export = (target, all) => {
  for (var name in all)
    __defProp(target, name, {
      get: all[name],
      enumerable: true,
      configurable: true,
      set: (newValue) => all[name] = () => newValue
    });
};

/* packages/grammar/src/rules/bunpro/jlpt5/verb-て-b.test.ts */
import { describe as describe2 } from "bun:test";

// packages/grammar/src/rules/bunpro/_test/engine.ts
import { afterAll, beforeAll } from "bun:test";

// packages/grammar/src/ginza/client.ts
import { spawn } from "node:child_process";
import { once } from "node:events";
import { createInterface } from "node:readline";
import { join as join2 } from "node:path";

// node_modules/zod/v3/external.js
var exports_external = {};
__export(exports_external, {
  void: () => voidType,
  util: () => util,
  unknown: () => unknownType,
  union: () => unionType,
  undefined: () => undefinedType,
  tuple: () => tupleType,
  transformer: () => effectsType,
  symbol: () => symbolType,
  string: () => stringType,
  strictObject: () => strictObjectType,
  setErrorMap: () => setErrorMap,
  set: () => setType,
  record: () => recordType,
  quotelessJson: () => quotelessJson,
  promise: () => promiseType,
  preprocess: () => preprocessType,
  pipeline: () => pipelineType,
  ostring: () => ostring,
  optional: () => optionalType,
  onumber: () => onumber,
  oboolean: () => oboolean,
  objectUtil: () => objectUtil,
  object: () => objectType,
  number: () => numberType,
  nullable: () => nullableType,
  null: () => nullType,
  never: () => neverType,
  nativeEnum: () => nativeEnumType,
  nan: () => nanType,
  map: () => mapType,
  makeIssue: () => makeIssue,
  literal: () => literalType,
  lazy: () => lazyType,
  late: () => late,
  isValid: () => isValid,
  isDirty: () => isDirty,
  isAsync: () => isAsync,
  isAborted: () => isAborted,
  intersection: () => intersectionType,
  instanceof: () => instanceOfType,
  getParsedType: () => getParsedType,
  getErrorMap: () => getErrorMap,
  function: () => functionType,
  enum: () => enumType,
  effect: () => effectsType,
  discriminatedUnion: () => discriminatedUnionType,
  defaultErrorMap: () => en_default,
  datetimeRegex: () => datetimeRegex,
  date: () => dateType,
  custom: () => custom,
  coerce: () => coerce,
  boolean: () => booleanType,
  bigint: () => bigIntType,
  array: () => arrayType,
  any: () => anyType,
  addIssueToContext: () => addIssueToContext,
  ZodVoid: () => ZodVoid,
  ZodUnknown: () => ZodUnknown,
  ZodUnion: () => ZodUnion,
  ZodUndefined: () => ZodUndefined,
  ZodType: () => ZodType,
  ZodTuple: () => ZodTuple,
  ZodTransformer: () => ZodEffects,
  ZodSymbol: () => ZodSymbol,
  ZodString: () => ZodString,
  ZodSet: () => ZodSet,
  ZodSchema: () => ZodType,
  ZodRecord: () => ZodRecord,
  ZodReadonly: () => ZodReadonly,
  ZodPromise: () => ZodPromise,
  ZodPipeline: () => ZodPipeline,
  ZodParsedType: () => ZodParsedType,
  ZodOptional: () => ZodOptional,
  ZodObject: () => ZodObject,
  ZodNumber: () => ZodNumber,
  ZodNullable: () => ZodNullable,
  ZodNull: () => ZodNull,
  ZodNever: () => ZodNever,
  ZodNativeEnum: () => ZodNativeEnum,
  ZodNaN: () => ZodNaN,
  ZodMap: () => ZodMap,
  ZodLiteral: () => ZodLiteral,
  ZodLazy: () => ZodLazy,
  ZodIssueCode: () => ZodIssueCode,
  ZodIntersection: () => ZodIntersection,
  ZodFunction: () => ZodFunction,
  ZodFirstPartyTypeKind: () => ZodFirstPartyTypeKind,
  ZodError: () => ZodError,
  ZodEnum: () => ZodEnum,
  ZodEffects: () => ZodEffects,
  ZodDiscriminatedUnion: () => ZodDiscriminatedUnion,
  ZodDefault: () => ZodDefault,
  ZodDate: () => ZodDate,
  ZodCatch: () => ZodCatch,
  ZodBranded: () => ZodBranded,
  ZodBoolean: () => ZodBoolean,
  ZodBigInt: () => ZodBigInt,
  ZodArray: () => ZodArray,
  ZodAny: () => ZodAny,
  Schema: () => ZodType,
  ParseStatus: () => ParseStatus,
  OK: () => OK,
  NEVER: () => NEVER,
  INVALID: () => INVALID,
  EMPTY_PATH: () => EMPTY_PATH,
  DIRTY: () => DIRTY,
  BRAND: () => BRAND
});

// node_modules/zod/v3/helpers/util.js
var util;
(function(util2) {
  util2.assertEqual = (_) => {};
  function assertIs(_arg) {}
  util2.assertIs = assertIs;
  function assertNever(_x) {
    throw new Error;
  }
  util2.assertNever = assertNever;
  util2.arrayToEnum = (items) => {
    const obj = {};
    for (const item of items) {
      obj[item] = item;
    }
    return obj;
  };
  util2.getValidEnumValues = (obj) => {
    const validKeys = util2.objectKeys(obj).filter((k) => typeof obj[obj[k]] !== "number");
    const filtered = {};
    for (const k of validKeys) {
      filtered[k] = obj[k];
    }
    return util2.objectValues(filtered);
  };
  util2.objectValues = (obj) => {
    return util2.objectKeys(obj).map(function(e) {
      return obj[e];
    });
  };
  util2.objectKeys = typeof Object.keys === "function" ? (obj) => Object.keys(obj) : (object) => {
    const keys = [];
    for (const key in object) {
      if (Object.prototype.hasOwnProperty.call(object, key)) {
        keys.push(key);
      }
    }
    return keys;
  };
  util2.find = (arr, checker) => {
    for (const item of arr) {
      if (checker(item))
        return item;
    }
    return;
  };
  util2.isInteger = typeof Number.isInteger === "function" ? (val) => Number.isInteger(val) : (val) => typeof val === "number" && Number.isFinite(val) && Math.floor(val) === val;
  function joinValues(array, separator = " | ") {
    return array.map((val) => typeof val === "string" ? `'${val}'` : val).join(separator);
  }
  util2.joinValues = joinValues;
  util2.jsonStringifyReplacer = (_, value) => {
    if (typeof value === "bigint") {
      return value.toString();
    }
    return value;
  };
})(util || (util = {}));
var objectUtil;
(function(objectUtil2) {
  objectUtil2.mergeShapes = (first, second) => {
    return {
      ...first,
      ...second
    };
  };
})(objectUtil || (objectUtil = {}));
var ZodParsedType = util.arrayToEnum([
  "string",
  "nan",
  "number",
  "integer",
  "float",
  "boolean",
  "date",
  "bigint",
  "symbol",
  "function",
  "undefined",
  "null",
  "array",
  "object",
  "unknown",
  "promise",
  "void",
  "never",
  "map",
  "set"
]);
var getParsedType = (data) => {
  const t = typeof data;
  switch (t) {
    case "undefined":
      return ZodParsedType.undefined;
    case "string":
      return ZodParsedType.string;
    case "number":
      return Number.isNaN(data) ? ZodParsedType.nan : ZodParsedType.number;
    case "boolean":
      return ZodParsedType.boolean;
    case "function":
      return ZodParsedType.function;
    case "bigint":
      return ZodParsedType.bigint;
    case "symbol":
      return ZodParsedType.symbol;
    case "object":
      if (Array.isArray(data)) {
        return ZodParsedType.array;
      }
      if (data === null) {
        return ZodParsedType.null;
      }
      if (data.then && typeof data.then === "function" && data.catch && typeof data.catch === "function") {
        return ZodParsedType.promise;
      }
      if (typeof Map !== "undefined" && data instanceof Map) {
        return ZodParsedType.map;
      }
      if (typeof Set !== "undefined" && data instanceof Set) {
        return ZodParsedType.set;
      }
      if (typeof Date !== "undefined" && data instanceof Date) {
        return ZodParsedType.date;
      }
      return ZodParsedType.object;
    default:
      return ZodParsedType.unknown;
  }
};

// node_modules/zod/v3/ZodError.js
var ZodIssueCode = util.arrayToEnum([
  "invalid_type",
  "invalid_literal",
  "custom",
  "invalid_union",
  "invalid_union_discriminator",
  "invalid_enum_value",
  "unrecognized_keys",
  "invalid_arguments",
  "invalid_return_type",
  "invalid_date",
  "invalid_string",
  "too_small",
  "too_big",
  "invalid_intersection_types",
  "not_multiple_of",
  "not_finite"
]);
var quotelessJson = (obj) => {
  const json = JSON.stringify(obj, null, 2);
  return json.replace(/"([^"]+)":/g, "$1:");
};

class ZodError extends Error {
  get errors() {
    return this.issues;
  }
  constructor(issues) {
    super();
    this.issues = [];
    this.addIssue = (sub) => {
      this.issues = [...this.issues, sub];
    };
    this.addIssues = (subs = []) => {
      this.issues = [...this.issues, ...subs];
    };
    const actualProto = new.target.prototype;
    if (Object.setPrototypeOf) {
      Object.setPrototypeOf(this, actualProto);
    } else {
      this.__proto__ = actualProto;
    }
    this.name = "ZodError";
    this.issues = issues;
  }
  format(_mapper) {
    const mapper = _mapper || function(issue) {
      return issue.message;
    };
    const fieldErrors = { _errors: [] };
    const processError = (error) => {
      for (const issue of error.issues) {
        if (issue.code === "invalid_union") {
          issue.unionErrors.map(processError);
        } else if (issue.code === "invalid_return_type") {
          processError(issue.returnTypeError);
        } else if (issue.code === "invalid_arguments") {
          processError(issue.argumentsError);
        } else if (issue.path.length === 0) {
          fieldErrors._errors.push(mapper(issue));
        } else {
          let curr = fieldErrors;
          let i = 0;
          while (i < issue.path.length) {
            const el = issue.path[i];
            const terminal = i === issue.path.length - 1;
            if (!terminal) {
              curr[el] = curr[el] || { _errors: [] };
            } else {
              curr[el] = curr[el] || { _errors: [] };
              curr[el]._errors.push(mapper(issue));
            }
            curr = curr[el];
            i++;
          }
        }
      }
    };
    processError(this);
    return fieldErrors;
  }
  static assert(value) {
    if (!(value instanceof ZodError)) {
      throw new Error(`Not a ZodError: ${value}`);
    }
  }
  toString() {
    return this.message;
  }
  get message() {
    return JSON.stringify(this.issues, util.jsonStringifyReplacer, 2);
  }
  get isEmpty() {
    return this.issues.length === 0;
  }
  flatten(mapper = (issue) => issue.message) {
    const fieldErrors = {};
    const formErrors = [];
    for (const sub of this.issues) {
      if (sub.path.length > 0) {
        const firstEl = sub.path[0];
        fieldErrors[firstEl] = fieldErrors[firstEl] || [];
        fieldErrors[firstEl].push(mapper(sub));
      } else {
        formErrors.push(mapper(sub));
      }
    }
    return { formErrors, fieldErrors };
  }
  get formErrors() {
    return this.flatten();
  }
}
ZodError.create = (issues) => {
  const error = new ZodError(issues);
  return error;
};

// node_modules/zod/v3/locales/en.js
var errorMap = (issue, _ctx) => {
  let message;
  switch (issue.code) {
    case ZodIssueCode.invalid_type:
      if (issue.received === ZodParsedType.undefined) {
        message = "Required";
      } else {
        message = `Expected ${issue.expected}, received ${issue.received}`;
      }
      break;
    case ZodIssueCode.invalid_literal:
      message = `Invalid literal value, expected ${JSON.stringify(issue.expected, util.jsonStringifyReplacer)}`;
      break;
    case ZodIssueCode.unrecognized_keys:
      message = `Unrecognized key(s) in object: ${util.joinValues(issue.keys, ", ")}`;
      break;
    case ZodIssueCode.invalid_union:
      message = `Invalid input`;
      break;
    case ZodIssueCode.invalid_union_discriminator:
      message = `Invalid discriminator value. Expected ${util.joinValues(issue.options)}`;
      break;
    case ZodIssueCode.invalid_enum_value:
      message = `Invalid enum value. Expected ${util.joinValues(issue.options)}, received '${issue.received}'`;
      break;
    case ZodIssueCode.invalid_arguments:
      message = `Invalid function arguments`;
      break;
    case ZodIssueCode.invalid_return_type:
      message = `Invalid function return type`;
      break;
    case ZodIssueCode.invalid_date:
      message = `Invalid date`;
      break;
    case ZodIssueCode.invalid_string:
      if (typeof issue.validation === "object") {
        if ("includes" in issue.validation) {
          message = `Invalid input: must include "${issue.validation.includes}"`;
          if (typeof issue.validation.position === "number") {
            message = `${message} at one or more positions greater than or equal to ${issue.validation.position}`;
          }
        } else if ("startsWith" in issue.validation) {
          message = `Invalid input: must start with "${issue.validation.startsWith}"`;
        } else if ("endsWith" in issue.validation) {
          message = `Invalid input: must end with "${issue.validation.endsWith}"`;
        } else {
          util.assertNever(issue.validation);
        }
      } else if (issue.validation !== "regex") {
        message = `Invalid ${issue.validation}`;
      } else {
        message = "Invalid";
      }
      break;
    case ZodIssueCode.too_small:
      if (issue.type === "array")
        message = `Array must contain ${issue.exact ? "exactly" : issue.inclusive ? `at least` : `more than`} ${issue.minimum} element(s)`;
      else if (issue.type === "string")
        message = `String must contain ${issue.exact ? "exactly" : issue.inclusive ? `at least` : `over`} ${issue.minimum} character(s)`;
      else if (issue.type === "number")
        message = `Number must be ${issue.exact ? `exactly equal to ` : issue.inclusive ? `greater than or equal to ` : `greater than `}${issue.minimum}`;
      else if (issue.type === "bigint")
        message = `Number must be ${issue.exact ? `exactly equal to ` : issue.inclusive ? `greater than or equal to ` : `greater than `}${issue.minimum}`;
      else if (issue.type === "date")
        message = `Date must be ${issue.exact ? `exactly equal to ` : issue.inclusive ? `greater than or equal to ` : `greater than `}${new Date(Number(issue.minimum))}`;
      else
        message = "Invalid input";
      break;
    case ZodIssueCode.too_big:
      if (issue.type === "array")
        message = `Array must contain ${issue.exact ? `exactly` : issue.inclusive ? `at most` : `less than`} ${issue.maximum} element(s)`;
      else if (issue.type === "string")
        message = `String must contain ${issue.exact ? `exactly` : issue.inclusive ? `at most` : `under`} ${issue.maximum} character(s)`;
      else if (issue.type === "number")
        message = `Number must be ${issue.exact ? `exactly` : issue.inclusive ? `less than or equal to` : `less than`} ${issue.maximum}`;
      else if (issue.type === "bigint")
        message = `BigInt must be ${issue.exact ? `exactly` : issue.inclusive ? `less than or equal to` : `less than`} ${issue.maximum}`;
      else if (issue.type === "date")
        message = `Date must be ${issue.exact ? `exactly` : issue.inclusive ? `smaller than or equal to` : `smaller than`} ${new Date(Number(issue.maximum))}`;
      else
        message = "Invalid input";
      break;
    case ZodIssueCode.custom:
      message = `Invalid input`;
      break;
    case ZodIssueCode.invalid_intersection_types:
      message = `Intersection results could not be merged`;
      break;
    case ZodIssueCode.not_multiple_of:
      message = `Number must be a multiple of ${issue.multipleOf}`;
      break;
    case ZodIssueCode.not_finite:
      message = "Number must be finite";
      break;
    default:
      message = _ctx.defaultError;
      util.assertNever(issue);
  }
  return { message };
};
var en_default = errorMap;

// node_modules/zod/v3/errors.js
var overrideErrorMap = en_default;
function setErrorMap(map) {
  overrideErrorMap = map;
}
function getErrorMap() {
  return overrideErrorMap;
}
// node_modules/zod/v3/helpers/parseUtil.js
var makeIssue = (params) => {
  const { data, path, errorMaps, issueData } = params;
  const fullPath = [...path, ...issueData.path || []];
  const fullIssue = {
    ...issueData,
    path: fullPath
  };
  if (issueData.message !== undefined) {
    return {
      ...issueData,
      path: fullPath,
      message: issueData.message
    };
  }
  let errorMessage = "";
  const maps = errorMaps.filter((m) => !!m).slice().reverse();
  for (const map of maps) {
    errorMessage = map(fullIssue, { data, defaultError: errorMessage }).message;
  }
  return {
    ...issueData,
    path: fullPath,
    message: errorMessage
  };
};
var EMPTY_PATH = [];
function addIssueToContext(ctx, issueData) {
  const overrideMap = getErrorMap();
  const issue = makeIssue({
    issueData,
    data: ctx.data,
    path: ctx.path,
    errorMaps: [
      ctx.common.contextualErrorMap,
      ctx.schemaErrorMap,
      overrideMap,
      overrideMap === en_default ? undefined : en_default
    ].filter((x) => !!x)
  });
  ctx.common.issues.push(issue);
}

class ParseStatus {
  constructor() {
    this.value = "valid";
  }
  dirty() {
    if (this.value === "valid")
      this.value = "dirty";
  }
  abort() {
    if (this.value !== "aborted")
      this.value = "aborted";
  }
  static mergeArray(status, results) {
    const arrayValue = [];
    for (const s of results) {
      if (s.status === "aborted")
        return INVALID;
      if (s.status === "dirty")
        status.dirty();
      arrayValue.push(s.value);
    }
    return { status: status.value, value: arrayValue };
  }
  static async mergeObjectAsync(status, pairs) {
    const syncPairs = [];
    for (const pair of pairs) {
      const key = await pair.key;
      const value = await pair.value;
      syncPairs.push({
        key,
        value
      });
    }
    return ParseStatus.mergeObjectSync(status, syncPairs);
  }
  static mergeObjectSync(status, pairs) {
    const finalObject = {};
    for (const pair of pairs) {
      const { key, value } = pair;
      if (key.status === "aborted")
        return INVALID;
      if (value.status === "aborted")
        return INVALID;
      if (key.status === "dirty")
        status.dirty();
      if (value.status === "dirty")
        status.dirty();
      if (key.value !== "__proto__" && (typeof value.value !== "undefined" || pair.alwaysSet)) {
        finalObject[key.value] = value.value;
      }
    }
    return { status: status.value, value: finalObject };
  }
}
var INVALID = Object.freeze({
  status: "aborted"
});
var DIRTY = (value) => ({ status: "dirty", value });
var OK = (value) => ({ status: "valid", value });
var isAborted = (x) => x.status === "aborted";
var isDirty = (x) => x.status === "dirty";
var isValid = (x) => x.status === "valid";
var isAsync = (x) => typeof Promise !== "undefined" && x instanceof Promise;
// node_modules/zod/v3/helpers/errorUtil.js
var errorUtil;
(function(errorUtil2) {
  errorUtil2.errToObj = (message) => typeof message === "string" ? { message } : message || {};
  errorUtil2.toString = (message) => typeof message === "string" ? message : message?.message;
})(errorUtil || (errorUtil = {}));

// node_modules/zod/v3/types.js
class ParseInputLazyPath {
  constructor(parent, value, path, key) {
    this._cachedPath = [];
    this.parent = parent;
    this.data = value;
    this._path = path;
    this._key = key;
  }
  get path() {
    if (!this._cachedPath.length) {
      if (Array.isArray(this._key)) {
        this._cachedPath.push(...this._path, ...this._key);
      } else {
        this._cachedPath.push(...this._path, this._key);
      }
    }
    return this._cachedPath;
  }
}
var handleResult = (ctx, result) => {
  if (isValid(result)) {
    return { success: true, data: result.value };
  } else {
    if (!ctx.common.issues.length) {
      throw new Error("Validation failed but no issues detected.");
    }
    return {
      success: false,
      get error() {
        if (this._error)
          return this._error;
        const error = new ZodError(ctx.common.issues);
        this._error = error;
        return this._error;
      }
    };
  }
};
function processCreateParams(params) {
  if (!params)
    return {};
  const { errorMap: errorMap2, invalid_type_error, required_error, description } = params;
  if (errorMap2 && (invalid_type_error || required_error)) {
    throw new Error(`Can't use "invalid_type_error" or "required_error" in conjunction with custom error map.`);
  }
  if (errorMap2)
    return { errorMap: errorMap2, description };
  const customMap = (iss, ctx) => {
    const { message } = params;
    if (iss.code === "invalid_enum_value") {
      return { message: message ?? ctx.defaultError };
    }
    if (typeof ctx.data === "undefined") {
      return { message: message ?? required_error ?? ctx.defaultError };
    }
    if (iss.code !== "invalid_type")
      return { message: ctx.defaultError };
    return { message: message ?? invalid_type_error ?? ctx.defaultError };
  };
  return { errorMap: customMap, description };
}

class ZodType {
  get description() {
    return this._def.description;
  }
  _getType(input) {
    return getParsedType(input.data);
  }
  _getOrReturnCtx(input, ctx) {
    return ctx || {
      common: input.parent.common,
      data: input.data,
      parsedType: getParsedType(input.data),
      schemaErrorMap: this._def.errorMap,
      path: input.path,
      parent: input.parent
    };
  }
  _processInputParams(input) {
    return {
      status: new ParseStatus,
      ctx: {
        common: input.parent.common,
        data: input.data,
        parsedType: getParsedType(input.data),
        schemaErrorMap: this._def.errorMap,
        path: input.path,
        parent: input.parent
      }
    };
  }
  _parseSync(input) {
    const result = this._parse(input);
    if (isAsync(result)) {
      throw new Error("Synchronous parse encountered promise.");
    }
    return result;
  }
  _parseAsync(input) {
    const result = this._parse(input);
    return Promise.resolve(result);
  }
  parse(data, params) {
    const result = this.safeParse(data, params);
    if (result.success)
      return result.data;
    throw result.error;
  }
  safeParse(data, params) {
    const ctx = {
      common: {
        issues: [],
        async: params?.async ?? false,
        contextualErrorMap: params?.errorMap
      },
      path: params?.path || [],
      schemaErrorMap: this._def.errorMap,
      parent: null,
      data,
      parsedType: getParsedType(data)
    };
    const result = this._parseSync({ data, path: ctx.path, parent: ctx });
    return handleResult(ctx, result);
  }
  "~validate"(data) {
    const ctx = {
      common: {
        issues: [],
        async: !!this["~standard"].async
      },
      path: [],
      schemaErrorMap: this._def.errorMap,
      parent: null,
      data,
      parsedType: getParsedType(data)
    };
    if (!this["~standard"].async) {
      try {
        const result = this._parseSync({ data, path: [], parent: ctx });
        return isValid(result) ? {
          value: result.value
        } : {
          issues: ctx.common.issues
        };
      } catch (err) {
        if (err?.message?.toLowerCase()?.includes("encountered")) {
          this["~standard"].async = true;
        }
        ctx.common = {
          issues: [],
          async: true
        };
      }
    }
    return this._parseAsync({ data, path: [], parent: ctx }).then((result) => isValid(result) ? {
      value: result.value
    } : {
      issues: ctx.common.issues
    });
  }
  async parseAsync(data, params) {
    const result = await this.safeParseAsync(data, params);
    if (result.success)
      return result.data;
    throw result.error;
  }
  async safeParseAsync(data, params) {
    const ctx = {
      common: {
        issues: [],
        contextualErrorMap: params?.errorMap,
        async: true
      },
      path: params?.path || [],
      schemaErrorMap: this._def.errorMap,
      parent: null,
      data,
      parsedType: getParsedType(data)
    };
    const maybeAsyncResult = this._parse({ data, path: ctx.path, parent: ctx });
    const result = await (isAsync(maybeAsyncResult) ? maybeAsyncResult : Promise.resolve(maybeAsyncResult));
    return handleResult(ctx, result);
  }
  refine(check, message) {
    const getIssueProperties = (val) => {
      if (typeof message === "string" || typeof message === "undefined") {
        return { message };
      } else if (typeof message === "function") {
        return message(val);
      } else {
        return message;
      }
    };
    return this._refinement((val, ctx) => {
      const result = check(val);
      const setError = () => ctx.addIssue({
        code: ZodIssueCode.custom,
        ...getIssueProperties(val)
      });
      if (typeof Promise !== "undefined" && result instanceof Promise) {
        return result.then((data) => {
          if (!data) {
            setError();
            return false;
          } else {
            return true;
          }
        });
      }
      if (!result) {
        setError();
        return false;
      } else {
        return true;
      }
    });
  }
  refinement(check, refinementData) {
    return this._refinement((val, ctx) => {
      if (!check(val)) {
        ctx.addIssue(typeof refinementData === "function" ? refinementData(val, ctx) : refinementData);
        return false;
      } else {
        return true;
      }
    });
  }
  _refinement(refinement) {
    return new ZodEffects({
      schema: this,
      typeName: ZodFirstPartyTypeKind.ZodEffects,
      effect: { type: "refinement", refinement }
    });
  }
  superRefine(refinement) {
    return this._refinement(refinement);
  }
  constructor(def) {
    this.spa = this.safeParseAsync;
    this._def = def;
    this.parse = this.parse.bind(this);
    this.safeParse = this.safeParse.bind(this);
    this.parseAsync = this.parseAsync.bind(this);
    this.safeParseAsync = this.safeParseAsync.bind(this);
    this.spa = this.spa.bind(this);
    this.refine = this.refine.bind(this);
    this.refinement = this.refinement.bind(this);
    this.superRefine = this.superRefine.bind(this);
    this.optional = this.optional.bind(this);
    this.nullable = this.nullable.bind(this);
    this.nullish = this.nullish.bind(this);
    this.array = this.array.bind(this);
    this.promise = this.promise.bind(this);
    this.or = this.or.bind(this);
    this.and = this.and.bind(this);
    this.transform = this.transform.bind(this);
    this.brand = this.brand.bind(this);
    this.default = this.default.bind(this);
    this.catch = this.catch.bind(this);
    this.describe = this.describe.bind(this);
    this.pipe = this.pipe.bind(this);
    this.readonly = this.readonly.bind(this);
    this.isNullable = this.isNullable.bind(this);
    this.isOptional = this.isOptional.bind(this);
    this["~standard"] = {
      version: 1,
      vendor: "zod",
      validate: (data) => this["~validate"](data)
    };
  }
  optional() {
    return ZodOptional.create(this, this._def);
  }
  nullable() {
    return ZodNullable.create(this, this._def);
  }
  nullish() {
    return this.nullable().optional();
  }
  array() {
    return ZodArray.create(this);
  }
  promise() {
    return ZodPromise.create(this, this._def);
  }
  or(option) {
    return ZodUnion.create([this, option], this._def);
  }
  and(incoming) {
    return ZodIntersection.create(this, incoming, this._def);
  }
  transform(transform) {
    return new ZodEffects({
      ...processCreateParams(this._def),
      schema: this,
      typeName: ZodFirstPartyTypeKind.ZodEffects,
      effect: { type: "transform", transform }
    });
  }
  default(def) {
    const defaultValueFunc = typeof def === "function" ? def : () => def;
    return new ZodDefault({
      ...processCreateParams(this._def),
      innerType: this,
      defaultValue: defaultValueFunc,
      typeName: ZodFirstPartyTypeKind.ZodDefault
    });
  }
  brand() {
    return new ZodBranded({
      typeName: ZodFirstPartyTypeKind.ZodBranded,
      type: this,
      ...processCreateParams(this._def)
    });
  }
  catch(def) {
    const catchValueFunc = typeof def === "function" ? def : () => def;
    return new ZodCatch({
      ...processCreateParams(this._def),
      innerType: this,
      catchValue: catchValueFunc,
      typeName: ZodFirstPartyTypeKind.ZodCatch
    });
  }
  describe(description) {
    const This = this.constructor;
    return new This({
      ...this._def,
      description
    });
  }
  pipe(target) {
    return ZodPipeline.create(this, target);
  }
  readonly() {
    return ZodReadonly.create(this);
  }
  isOptional() {
    return this.safeParse(undefined).success;
  }
  isNullable() {
    return this.safeParse(null).success;
  }
}
var cuidRegex = /^c[^\s-]{8,}$/i;
var cuid2Regex = /^[0-9a-z]+$/;
var ulidRegex = /^[0-9A-HJKMNP-TV-Z]{26}$/i;
var uuidRegex = /^[0-9a-fA-F]{8}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{12}$/i;
var nanoidRegex = /^[a-z0-9_-]{21}$/i;
var jwtRegex = /^[A-Za-z0-9-_]+\.[A-Za-z0-9-_]+\.[A-Za-z0-9-_]*$/;
var durationRegex = /^[-+]?P(?!$)(?:(?:[-+]?\d+Y)|(?:[-+]?\d+[.,]\d+Y$))?(?:(?:[-+]?\d+M)|(?:[-+]?\d+[.,]\d+M$))?(?:(?:[-+]?\d+W)|(?:[-+]?\d+[.,]\d+W$))?(?:(?:[-+]?\d+D)|(?:[-+]?\d+[.,]\d+D$))?(?:T(?=[\d+-])(?:(?:[-+]?\d+H)|(?:[-+]?\d+[.,]\d+H$))?(?:(?:[-+]?\d+M)|(?:[-+]?\d+[.,]\d+M$))?(?:[-+]?\d+(?:[.,]\d+)?S)?)??$/;
var emailRegex = /^(?!\.)(?!.*\.\.)([A-Z0-9_'+\-\.]*)[A-Z0-9_+-]@([A-Z0-9][A-Z0-9\-]*\.)+[A-Z]{2,}$/i;
var _emojiRegex = `^(\\p{Extended_Pictographic}|\\p{Emoji_Component})+$`;
var emojiRegex;
var ipv4Regex = /^(?:(?:25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9][0-9]|[0-9])\.){3}(?:25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9][0-9]|[0-9])$/;
var ipv4CidrRegex = /^(?:(?:25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9][0-9]|[0-9])\.){3}(?:25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9][0-9]|[0-9])\/(3[0-2]|[12]?[0-9])$/;
var ipv6Regex = /^(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))$/;
var ipv6CidrRegex = /^(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))\/(12[0-8]|1[01][0-9]|[1-9]?[0-9])$/;
var base64Regex = /^([0-9a-zA-Z+/]{4})*(([0-9a-zA-Z+/]{2}==)|([0-9a-zA-Z+/]{3}=))?$/;
var base64urlRegex = /^([0-9a-zA-Z-_]{4})*(([0-9a-zA-Z-_]{2}(==)?)|([0-9a-zA-Z-_]{3}(=)?))?$/;
var dateRegexSource = `((\\d\\d[2468][048]|\\d\\d[13579][26]|\\d\\d0[48]|[02468][048]00|[13579][26]00)-02-29|\\d{4}-((0[13578]|1[02])-(0[1-9]|[12]\\d|3[01])|(0[469]|11)-(0[1-9]|[12]\\d|30)|(02)-(0[1-9]|1\\d|2[0-8])))`;
var dateRegex = new RegExp(`^${dateRegexSource}$`);
function timeRegexSource(args) {
  let secondsRegexSource = `[0-5]\\d`;
  if (args.precision) {
    secondsRegexSource = `${secondsRegexSource}\\.\\d{${args.precision}}`;
  } else if (args.precision == null) {
    secondsRegexSource = `${secondsRegexSource}(\\.\\d+)?`;
  }
  const secondsQuantifier = args.precision ? "+" : "?";
  return `([01]\\d|2[0-3]):[0-5]\\d(:${secondsRegexSource})${secondsQuantifier}`;
}
function timeRegex(args) {
  return new RegExp(`^${timeRegexSource(args)}$`);
}
function datetimeRegex(args) {
  let regex = `${dateRegexSource}T${timeRegexSource(args)}`;
  const opts = [];
  opts.push(args.local ? `Z?` : `Z`);
  if (args.offset)
    opts.push(`([+-]\\d{2}:?\\d{2})`);
  regex = `${regex}(${opts.join("|")})`;
  return new RegExp(`^${regex}$`);
}
function isValidIP(ip, version) {
  if ((version === "v4" || !version) && ipv4Regex.test(ip)) {
    return true;
  }
  if ((version === "v6" || !version) && ipv6Regex.test(ip)) {
    return true;
  }
  return false;
}
function isValidJWT(jwt, alg) {
  if (!jwtRegex.test(jwt))
    return false;
  try {
    const [header] = jwt.split(".");
    if (!header)
      return false;
    const base64 = header.replace(/-/g, "+").replace(/_/g, "/").padEnd(header.length + (4 - header.length % 4) % 4, "=");
    const decoded = JSON.parse(atob(base64));
    if (typeof decoded !== "object" || decoded === null)
      return false;
    if ("typ" in decoded && decoded?.typ !== "JWT")
      return false;
    if (!decoded.alg)
      return false;
    if (alg && decoded.alg !== alg)
      return false;
    return true;
  } catch {
    return false;
  }
}
function isValidCidr(ip, version) {
  if ((version === "v4" || !version) && ipv4CidrRegex.test(ip)) {
    return true;
  }
  if ((version === "v6" || !version) && ipv6CidrRegex.test(ip)) {
    return true;
  }
  return false;
}

class ZodString extends ZodType {
  _parse(input) {
    if (this._def.coerce) {
      input.data = String(input.data);
    }
    const parsedType = this._getType(input);
    if (parsedType !== ZodParsedType.string) {
      const ctx2 = this._getOrReturnCtx(input);
      addIssueToContext(ctx2, {
        code: ZodIssueCode.invalid_type,
        expected: ZodParsedType.string,
        received: ctx2.parsedType
      });
      return INVALID;
    }
    const status = new ParseStatus;
    let ctx = undefined;
    for (const check of this._def.checks) {
      if (check.kind === "min") {
        if (input.data.length < check.value) {
          ctx = this._getOrReturnCtx(input, ctx);
          addIssueToContext(ctx, {
            code: ZodIssueCode.too_small,
            minimum: check.value,
            type: "string",
            inclusive: true,
            exact: false,
            message: check.message
          });
          status.dirty();
        }
      } else if (check.kind === "max") {
        if (input.data.length > check.value) {
          ctx = this._getOrReturnCtx(input, ctx);
          addIssueToContext(ctx, {
            code: ZodIssueCode.too_big,
            maximum: check.value,
            type: "string",
            inclusive: true,
            exact: false,
            message: check.message
          });
          status.dirty();
        }
      } else if (check.kind === "length") {
        const tooBig = input.data.length > check.value;
        const tooSmall = input.data.length < check.value;
        if (tooBig || tooSmall) {
          ctx = this._getOrReturnCtx(input, ctx);
          if (tooBig) {
            addIssueToContext(ctx, {
              code: ZodIssueCode.too_big,
              maximum: check.value,
              type: "string",
              inclusive: true,
              exact: true,
              message: check.message
            });
          } else if (tooSmall) {
            addIssueToContext(ctx, {
              code: ZodIssueCode.too_small,
              minimum: check.value,
              type: "string",
              inclusive: true,
              exact: true,
              message: check.message
            });
          }
          status.dirty();
        }
      } else if (check.kind === "email") {
        if (!emailRegex.test(input.data)) {
          ctx = this._getOrReturnCtx(input, ctx);
          addIssueToContext(ctx, {
            validation: "email",
            code: ZodIssueCode.invalid_string,
            message: check.message
          });
          status.dirty();
        }
      } else if (check.kind === "emoji") {
        if (!emojiRegex) {
          emojiRegex = new RegExp(_emojiRegex, "u");
        }
        if (!emojiRegex.test(input.data)) {
          ctx = this._getOrReturnCtx(input, ctx);
          addIssueToContext(ctx, {
            validation: "emoji",
            code: ZodIssueCode.invalid_string,
            message: check.message
          });
          status.dirty();
        }
      } else if (check.kind === "uuid") {
        if (!uuidRegex.test(input.data)) {
          ctx = this._getOrReturnCtx(input, ctx);
          addIssueToContext(ctx, {
            validation: "uuid",
            code: ZodIssueCode.invalid_string,
            message: check.message
          });
          status.dirty();
        }
      } else if (check.kind === "nanoid") {
        if (!nanoidRegex.test(input.data)) {
          ctx = this._getOrReturnCtx(input, ctx);
          addIssueToContext(ctx, {
            validation: "nanoid",
            code: ZodIssueCode.invalid_string,
            message: check.message
          });
          status.dirty();
        }
      } else if (check.kind === "cuid") {
        if (!cuidRegex.test(input.data)) {
          ctx = this._getOrReturnCtx(input, ctx);
          addIssueToContext(ctx, {
            validation: "cuid",
            code: ZodIssueCode.invalid_string,
            message: check.message
          });
          status.dirty();
        }
      } else if (check.kind === "cuid2") {
        if (!cuid2Regex.test(input.data)) {
          ctx = this._getOrReturnCtx(input, ctx);
          addIssueToContext(ctx, {
            validation: "cuid2",
            code: ZodIssueCode.invalid_string,
            message: check.message
          });
          status.dirty();
        }
      } else if (check.kind === "ulid") {
        if (!ulidRegex.test(input.data)) {
          ctx = this._getOrReturnCtx(input, ctx);
          addIssueToContext(ctx, {
            validation: "ulid",
            code: ZodIssueCode.invalid_string,
            message: check.message
          });
          status.dirty();
        }
      } else if (check.kind === "url") {
        try {
          new URL(input.data);
        } catch {
          ctx = this._getOrReturnCtx(input, ctx);
          addIssueToContext(ctx, {
            validation: "url",
            code: ZodIssueCode.invalid_string,
            message: check.message
          });
          status.dirty();
        }
      } else if (check.kind === "regex") {
        check.regex.lastIndex = 0;
        const testResult = check.regex.test(input.data);
        if (!testResult) {
          ctx = this._getOrReturnCtx(input, ctx);
          addIssueToContext(ctx, {
            validation: "regex",
            code: ZodIssueCode.invalid_string,
            message: check.message
          });
          status.dirty();
        }
      } else if (check.kind === "trim") {
        input.data = input.data.trim();
      } else if (check.kind === "includes") {
        if (!input.data.includes(check.value, check.position)) {
          ctx = this._getOrReturnCtx(input, ctx);
          addIssueToContext(ctx, {
            code: ZodIssueCode.invalid_string,
            validation: { includes: check.value, position: check.position },
            message: check.message
          });
          status.dirty();
        }
      } else if (check.kind === "toLowerCase") {
        input.data = input.data.toLowerCase();
      } else if (check.kind === "toUpperCase") {
        input.data = input.data.toUpperCase();
      } else if (check.kind === "startsWith") {
        if (!input.data.startsWith(check.value)) {
          ctx = this._getOrReturnCtx(input, ctx);
          addIssueToContext(ctx, {
            code: ZodIssueCode.invalid_string,
            validation: { startsWith: check.value },
            message: check.message
          });
          status.dirty();
        }
      } else if (check.kind === "endsWith") {
        if (!input.data.endsWith(check.value)) {
          ctx = this._getOrReturnCtx(input, ctx);
          addIssueToContext(ctx, {
            code: ZodIssueCode.invalid_string,
            validation: { endsWith: check.value },
            message: check.message
          });
          status.dirty();
        }
      } else if (check.kind === "datetime") {
        const regex = datetimeRegex(check);
        if (!regex.test(input.data)) {
          ctx = this._getOrReturnCtx(input, ctx);
          addIssueToContext(ctx, {
            code: ZodIssueCode.invalid_string,
            validation: "datetime",
            message: check.message
          });
          status.dirty();
        }
      } else if (check.kind === "date") {
        const regex = dateRegex;
        if (!regex.test(input.data)) {
          ctx = this._getOrReturnCtx(input, ctx);
          addIssueToContext(ctx, {
            code: ZodIssueCode.invalid_string,
            validation: "date",
            message: check.message
          });
          status.dirty();
        }
      } else if (check.kind === "time") {
        const regex = timeRegex(check);
        if (!regex.test(input.data)) {
          ctx = this._getOrReturnCtx(input, ctx);
          addIssueToContext(ctx, {
            code: ZodIssueCode.invalid_string,
            validation: "time",
            message: check.message
          });
          status.dirty();
        }
      } else if (check.kind === "duration") {
        if (!durationRegex.test(input.data)) {
          ctx = this._getOrReturnCtx(input, ctx);
          addIssueToContext(ctx, {
            validation: "duration",
            code: ZodIssueCode.invalid_string,
            message: check.message
          });
          status.dirty();
        }
      } else if (check.kind === "ip") {
        if (!isValidIP(input.data, check.version)) {
          ctx = this._getOrReturnCtx(input, ctx);
          addIssueToContext(ctx, {
            validation: "ip",
            code: ZodIssueCode.invalid_string,
            message: check.message
          });
          status.dirty();
        }
      } else if (check.kind === "jwt") {
        if (!isValidJWT(input.data, check.alg)) {
          ctx = this._getOrReturnCtx(input, ctx);
          addIssueToContext(ctx, {
            validation: "jwt",
            code: ZodIssueCode.invalid_string,
            message: check.message
          });
          status.dirty();
        }
      } else if (check.kind === "cidr") {
        if (!isValidCidr(input.data, check.version)) {
          ctx = this._getOrReturnCtx(input, ctx);
          addIssueToContext(ctx, {
            validation: "cidr",
            code: ZodIssueCode.invalid_string,
            message: check.message
          });
          status.dirty();
        }
      } else if (check.kind === "base64") {
        if (!base64Regex.test(input.data)) {
          ctx = this._getOrReturnCtx(input, ctx);
          addIssueToContext(ctx, {
            validation: "base64",
            code: ZodIssueCode.invalid_string,
            message: check.message
          });
          status.dirty();
        }
      } else if (check.kind === "base64url") {
        if (!base64urlRegex.test(input.data)) {
          ctx = this._getOrReturnCtx(input, ctx);
          addIssueToContext(ctx, {
            validation: "base64url",
            code: ZodIssueCode.invalid_string,
            message: check.message
          });
          status.dirty();
        }
      } else {
        util.assertNever(check);
      }
    }
    return { status: status.value, value: input.data };
  }
  _regex(regex, validation, message) {
    return this.refinement((data) => regex.test(data), {
      validation,
      code: ZodIssueCode.invalid_string,
      ...errorUtil.errToObj(message)
    });
  }
  _addCheck(check) {
    return new ZodString({
      ...this._def,
      checks: [...this._def.checks, check]
    });
  }
  email(message) {
    return this._addCheck({ kind: "email", ...errorUtil.errToObj(message) });
  }
  url(message) {
    return this._addCheck({ kind: "url", ...errorUtil.errToObj(message) });
  }
  emoji(message) {
    return this._addCheck({ kind: "emoji", ...errorUtil.errToObj(message) });
  }
  uuid(message) {
    return this._addCheck({ kind: "uuid", ...errorUtil.errToObj(message) });
  }
  nanoid(message) {
    return this._addCheck({ kind: "nanoid", ...errorUtil.errToObj(message) });
  }
  cuid(message) {
    return this._addCheck({ kind: "cuid", ...errorUtil.errToObj(message) });
  }
  cuid2(message) {
    return this._addCheck({ kind: "cuid2", ...errorUtil.errToObj(message) });
  }
  ulid(message) {
    return this._addCheck({ kind: "ulid", ...errorUtil.errToObj(message) });
  }
  base64(message) {
    return this._addCheck({ kind: "base64", ...errorUtil.errToObj(message) });
  }
  base64url(message) {
    return this._addCheck({
      kind: "base64url",
      ...errorUtil.errToObj(message)
    });
  }
  jwt(options) {
    return this._addCheck({ kind: "jwt", ...errorUtil.errToObj(options) });
  }
  ip(options) {
    return this._addCheck({ kind: "ip", ...errorUtil.errToObj(options) });
  }
  cidr(options) {
    return this._addCheck({ kind: "cidr", ...errorUtil.errToObj(options) });
  }
  datetime(options) {
    if (typeof options === "string") {
      return this._addCheck({
        kind: "datetime",
        precision: null,
        offset: false,
        local: false,
        message: options
      });
    }
    return this._addCheck({
      kind: "datetime",
      precision: typeof options?.precision === "undefined" ? null : options?.precision,
      offset: options?.offset ?? false,
      local: options?.local ?? false,
      ...errorUtil.errToObj(options?.message)
    });
  }
  date(message) {
    return this._addCheck({ kind: "date", message });
  }
  time(options) {
    if (typeof options === "string") {
      return this._addCheck({
        kind: "time",
        precision: null,
        message: options
      });
    }
    return this._addCheck({
      kind: "time",
      precision: typeof options?.precision === "undefined" ? null : options?.precision,
      ...errorUtil.errToObj(options?.message)
    });
  }
  duration(message) {
    return this._addCheck({ kind: "duration", ...errorUtil.errToObj(message) });
  }
  regex(regex, message) {
    return this._addCheck({
      kind: "regex",
      regex,
      ...errorUtil.errToObj(message)
    });
  }
  includes(value, options) {
    return this._addCheck({
      kind: "includes",
      value,
      position: options?.position,
      ...errorUtil.errToObj(options?.message)
    });
  }
  startsWith(value, message) {
    return this._addCheck({
      kind: "startsWith",
      value,
      ...errorUtil.errToObj(message)
    });
  }
  endsWith(value, message) {
    return this._addCheck({
      kind: "endsWith",
      value,
      ...errorUtil.errToObj(message)
    });
  }
  min(minLength, message) {
    return this._addCheck({
      kind: "min",
      value: minLength,
      ...errorUtil.errToObj(message)
    });
  }
  max(maxLength, message) {
    return this._addCheck({
      kind: "max",
      value: maxLength,
      ...errorUtil.errToObj(message)
    });
  }
  length(len, message) {
    return this._addCheck({
      kind: "length",
      value: len,
      ...errorUtil.errToObj(message)
    });
  }
  nonempty(message) {
    return this.min(1, errorUtil.errToObj(message));
  }
  trim() {
    return new ZodString({
      ...this._def,
      checks: [...this._def.checks, { kind: "trim" }]
    });
  }
  toLowerCase() {
    return new ZodString({
      ...this._def,
      checks: [...this._def.checks, { kind: "toLowerCase" }]
    });
  }
  toUpperCase() {
    return new ZodString({
      ...this._def,
      checks: [...this._def.checks, { kind: "toUpperCase" }]
    });
  }
  get isDatetime() {
    return !!this._def.checks.find((ch) => ch.kind === "datetime");
  }
  get isDate() {
    return !!this._def.checks.find((ch) => ch.kind === "date");
  }
  get isTime() {
    return !!this._def.checks.find((ch) => ch.kind === "time");
  }
  get isDuration() {
    return !!this._def.checks.find((ch) => ch.kind === "duration");
  }
  get isEmail() {
    return !!this._def.checks.find((ch) => ch.kind === "email");
  }
  get isURL() {
    return !!this._def.checks.find((ch) => ch.kind === "url");
  }
  get isEmoji() {
    return !!this._def.checks.find((ch) => ch.kind === "emoji");
  }
  get isUUID() {
    return !!this._def.checks.find((ch) => ch.kind === "uuid");
  }
  get isNANOID() {
    return !!this._def.checks.find((ch) => ch.kind === "nanoid");
  }
  get isCUID() {
    return !!this._def.checks.find((ch) => ch.kind === "cuid");
  }
  get isCUID2() {
    return !!this._def.checks.find((ch) => ch.kind === "cuid2");
  }
  get isULID() {
    return !!this._def.checks.find((ch) => ch.kind === "ulid");
  }
  get isIP() {
    return !!this._def.checks.find((ch) => ch.kind === "ip");
  }
  get isCIDR() {
    return !!this._def.checks.find((ch) => ch.kind === "cidr");
  }
  get isBase64() {
    return !!this._def.checks.find((ch) => ch.kind === "base64");
  }
  get isBase64url() {
    return !!this._def.checks.find((ch) => ch.kind === "base64url");
  }
  get minLength() {
    let min = null;
    for (const ch of this._def.checks) {
      if (ch.kind === "min") {
        if (min === null || ch.value > min)
          min = ch.value;
      }
    }
    return min;
  }
  get maxLength() {
    let max = null;
    for (const ch of this._def.checks) {
      if (ch.kind === "max") {
        if (max === null || ch.value < max)
          max = ch.value;
      }
    }
    return max;
  }
}
ZodString.create = (params) => {
  return new ZodString({
    checks: [],
    typeName: ZodFirstPartyTypeKind.ZodString,
    coerce: params?.coerce ?? false,
    ...processCreateParams(params)
  });
};
function floatSafeRemainder(val, step) {
  const valDecCount = (val.toString().split(".")[1] || "").length;
  const stepDecCount = (step.toString().split(".")[1] || "").length;
  const decCount = valDecCount > stepDecCount ? valDecCount : stepDecCount;
  const valInt = Number.parseInt(val.toFixed(decCount).replace(".", ""));
  const stepInt = Number.parseInt(step.toFixed(decCount).replace(".", ""));
  return valInt % stepInt / 10 ** decCount;
}

class ZodNumber extends ZodType {
  constructor() {
    super(...arguments);
    this.min = this.gte;
    this.max = this.lte;
    this.step = this.multipleOf;
  }
  _parse(input) {
    if (this._def.coerce) {
      input.data = Number(input.data);
    }
    const parsedType = this._getType(input);
    if (parsedType !== ZodParsedType.number) {
      const ctx2 = this._getOrReturnCtx(input);
      addIssueToContext(ctx2, {
        code: ZodIssueCode.invalid_type,
        expected: ZodParsedType.number,
        received: ctx2.parsedType
      });
      return INVALID;
    }
    let ctx = undefined;
    const status = new ParseStatus;
    for (const check of this._def.checks) {
      if (check.kind === "int") {
        if (!util.isInteger(input.data)) {
          ctx = this._getOrReturnCtx(input, ctx);
          addIssueToContext(ctx, {
            code: ZodIssueCode.invalid_type,
            expected: "integer",
            received: "float",
            message: check.message
          });
          status.dirty();
        }
      } else if (check.kind === "min") {
        const tooSmall = check.inclusive ? input.data < check.value : input.data <= check.value;
        if (tooSmall) {
          ctx = this._getOrReturnCtx(input, ctx);
          addIssueToContext(ctx, {
            code: ZodIssueCode.too_small,
            minimum: check.value,
            type: "number",
            inclusive: check.inclusive,
            exact: false,
            message: check.message
          });
          status.dirty();
        }
      } else if (check.kind === "max") {
        const tooBig = check.inclusive ? input.data > check.value : input.data >= check.value;
        if (tooBig) {
          ctx = this._getOrReturnCtx(input, ctx);
          addIssueToContext(ctx, {
            code: ZodIssueCode.too_big,
            maximum: check.value,
            type: "number",
            inclusive: check.inclusive,
            exact: false,
            message: check.message
          });
          status.dirty();
        }
      } else if (check.kind === "multipleOf") {
        if (floatSafeRemainder(input.data, check.value) !== 0) {
          ctx = this._getOrReturnCtx(input, ctx);
          addIssueToContext(ctx, {
            code: ZodIssueCode.not_multiple_of,
            multipleOf: check.value,
            message: check.message
          });
          status.dirty();
        }
      } else if (check.kind === "finite") {
        if (!Number.isFinite(input.data)) {
          ctx = this._getOrReturnCtx(input, ctx);
          addIssueToContext(ctx, {
            code: ZodIssueCode.not_finite,
            message: check.message
          });
          status.dirty();
        }
      } else {
        util.assertNever(check);
      }
    }
    return { status: status.value, value: input.data };
  }
  gte(value, message) {
    return this.setLimit("min", value, true, errorUtil.toString(message));
  }
  gt(value, message) {
    return this.setLimit("min", value, false, errorUtil.toString(message));
  }
  lte(value, message) {
    return this.setLimit("max", value, true, errorUtil.toString(message));
  }
  lt(value, message) {
    return this.setLimit("max", value, false, errorUtil.toString(message));
  }
  setLimit(kind, value, inclusive, message) {
    return new ZodNumber({
      ...this._def,
      checks: [
        ...this._def.checks,
        {
          kind,
          value,
          inclusive,
          message: errorUtil.toString(message)
        }
      ]
    });
  }
  _addCheck(check) {
    return new ZodNumber({
      ...this._def,
      checks: [...this._def.checks, check]
    });
  }
  int(message) {
    return this._addCheck({
      kind: "int",
      message: errorUtil.toString(message)
    });
  }
  positive(message) {
    return this._addCheck({
      kind: "min",
      value: 0,
      inclusive: false,
      message: errorUtil.toString(message)
    });
  }
  negative(message) {
    return this._addCheck({
      kind: "max",
      value: 0,
      inclusive: false,
      message: errorUtil.toString(message)
    });
  }
  nonpositive(message) {
    return this._addCheck({
      kind: "max",
      value: 0,
      inclusive: true,
      message: errorUtil.toString(message)
    });
  }
  nonnegative(message) {
    return this._addCheck({
      kind: "min",
      value: 0,
      inclusive: true,
      message: errorUtil.toString(message)
    });
  }
  multipleOf(value, message) {
    return this._addCheck({
      kind: "multipleOf",
      value,
      message: errorUtil.toString(message)
    });
  }
  finite(message) {
    return this._addCheck({
      kind: "finite",
      message: errorUtil.toString(message)
    });
  }
  safe(message) {
    return this._addCheck({
      kind: "min",
      inclusive: true,
      value: Number.MIN_SAFE_INTEGER,
      message: errorUtil.toString(message)
    })._addCheck({
      kind: "max",
      inclusive: true,
      value: Number.MAX_SAFE_INTEGER,
      message: errorUtil.toString(message)
    });
  }
  get minValue() {
    let min = null;
    for (const ch of this._def.checks) {
      if (ch.kind === "min") {
        if (min === null || ch.value > min)
          min = ch.value;
      }
    }
    return min;
  }
  get maxValue() {
    let max = null;
    for (const ch of this._def.checks) {
      if (ch.kind === "max") {
        if (max === null || ch.value < max)
          max = ch.value;
      }
    }
    return max;
  }
  get isInt() {
    return !!this._def.checks.find((ch) => ch.kind === "int" || ch.kind === "multipleOf" && util.isInteger(ch.value));
  }
  get isFinite() {
    let max = null;
    let min = null;
    for (const ch of this._def.checks) {
      if (ch.kind === "finite" || ch.kind === "int" || ch.kind === "multipleOf") {
        return true;
      } else if (ch.kind === "min") {
        if (min === null || ch.value > min)
          min = ch.value;
      } else if (ch.kind === "max") {
        if (max === null || ch.value < max)
          max = ch.value;
      }
    }
    return Number.isFinite(min) && Number.isFinite(max);
  }
}
ZodNumber.create = (params) => {
  return new ZodNumber({
    checks: [],
    typeName: ZodFirstPartyTypeKind.ZodNumber,
    coerce: params?.coerce || false,
    ...processCreateParams(params)
  });
};

class ZodBigInt extends ZodType {
  constructor() {
    super(...arguments);
    this.min = this.gte;
    this.max = this.lte;
  }
  _parse(input) {
    if (this._def.coerce) {
      try {
        input.data = BigInt(input.data);
      } catch {
        return this._getInvalidInput(input);
      }
    }
    const parsedType = this._getType(input);
    if (parsedType !== ZodParsedType.bigint) {
      return this._getInvalidInput(input);
    }
    let ctx = undefined;
    const status = new ParseStatus;
    for (const check of this._def.checks) {
      if (check.kind === "min") {
        const tooSmall = check.inclusive ? input.data < check.value : input.data <= check.value;
        if (tooSmall) {
          ctx = this._getOrReturnCtx(input, ctx);
          addIssueToContext(ctx, {
            code: ZodIssueCode.too_small,
            type: "bigint",
            minimum: check.value,
            inclusive: check.inclusive,
            message: check.message
          });
          status.dirty();
        }
      } else if (check.kind === "max") {
        const tooBig = check.inclusive ? input.data > check.value : input.data >= check.value;
        if (tooBig) {
          ctx = this._getOrReturnCtx(input, ctx);
          addIssueToContext(ctx, {
            code: ZodIssueCode.too_big,
            type: "bigint",
            maximum: check.value,
            inclusive: check.inclusive,
            message: check.message
          });
          status.dirty();
        }
      } else if (check.kind === "multipleOf") {
        if (input.data % check.value !== BigInt(0)) {
          ctx = this._getOrReturnCtx(input, ctx);
          addIssueToContext(ctx, {
            code: ZodIssueCode.not_multiple_of,
            multipleOf: check.value,
            message: check.message
          });
          status.dirty();
        }
      } else {
        util.assertNever(check);
      }
    }
    return { status: status.value, value: input.data };
  }
  _getInvalidInput(input) {
    const ctx = this._getOrReturnCtx(input);
    addIssueToContext(ctx, {
      code: ZodIssueCode.invalid_type,
      expected: ZodParsedType.bigint,
      received: ctx.parsedType
    });
    return INVALID;
  }
  gte(value, message) {
    return this.setLimit("min", value, true, errorUtil.toString(message));
  }
  gt(value, message) {
    return this.setLimit("min", value, false, errorUtil.toString(message));
  }
  lte(value, message) {
    return this.setLimit("max", value, true, errorUtil.toString(message));
  }
  lt(value, message) {
    return this.setLimit("max", value, false, errorUtil.toString(message));
  }
  setLimit(kind, value, inclusive, message) {
    return new ZodBigInt({
      ...this._def,
      checks: [
        ...this._def.checks,
        {
          kind,
          value,
          inclusive,
          message: errorUtil.toString(message)
        }
      ]
    });
  }
  _addCheck(check) {
    return new ZodBigInt({
      ...this._def,
      checks: [...this._def.checks, check]
    });
  }
  positive(message) {
    return this._addCheck({
      kind: "min",
      value: BigInt(0),
      inclusive: false,
      message: errorUtil.toString(message)
    });
  }
  negative(message) {
    return this._addCheck({
      kind: "max",
      value: BigInt(0),
      inclusive: false,
      message: errorUtil.toString(message)
    });
  }
  nonpositive(message) {
    return this._addCheck({
      kind: "max",
      value: BigInt(0),
      inclusive: true,
      message: errorUtil.toString(message)
    });
  }
  nonnegative(message) {
    return this._addCheck({
      kind: "min",
      value: BigInt(0),
      inclusive: true,
      message: errorUtil.toString(message)
    });
  }
  multipleOf(value, message) {
    return this._addCheck({
      kind: "multipleOf",
      value,
      message: errorUtil.toString(message)
    });
  }
  get minValue() {
    let min = null;
    for (const ch of this._def.checks) {
      if (ch.kind === "min") {
        if (min === null || ch.value > min)
          min = ch.value;
      }
    }
    return min;
  }
  get maxValue() {
    let max = null;
    for (const ch of this._def.checks) {
      if (ch.kind === "max") {
        if (max === null || ch.value < max)
          max = ch.value;
      }
    }
    return max;
  }
}
ZodBigInt.create = (params) => {
  return new ZodBigInt({
    checks: [],
    typeName: ZodFirstPartyTypeKind.ZodBigInt,
    coerce: params?.coerce ?? false,
    ...processCreateParams(params)
  });
};

class ZodBoolean extends ZodType {
  _parse(input) {
    if (this._def.coerce) {
      input.data = Boolean(input.data);
    }
    const parsedType = this._getType(input);
    if (parsedType !== ZodParsedType.boolean) {
      const ctx = this._getOrReturnCtx(input);
      addIssueToContext(ctx, {
        code: ZodIssueCode.invalid_type,
        expected: ZodParsedType.boolean,
        received: ctx.parsedType
      });
      return INVALID;
    }
    return OK(input.data);
  }
}
ZodBoolean.create = (params) => {
  return new ZodBoolean({
    typeName: ZodFirstPartyTypeKind.ZodBoolean,
    coerce: params?.coerce || false,
    ...processCreateParams(params)
  });
};

class ZodDate extends ZodType {
  _parse(input) {
    if (this._def.coerce) {
      input.data = new Date(input.data);
    }
    const parsedType = this._getType(input);
    if (parsedType !== ZodParsedType.date) {
      const ctx2 = this._getOrReturnCtx(input);
      addIssueToContext(ctx2, {
        code: ZodIssueCode.invalid_type,
        expected: ZodParsedType.date,
        received: ctx2.parsedType
      });
      return INVALID;
    }
    if (Number.isNaN(input.data.getTime())) {
      const ctx2 = this._getOrReturnCtx(input);
      addIssueToContext(ctx2, {
        code: ZodIssueCode.invalid_date
      });
      return INVALID;
    }
    const status = new ParseStatus;
    let ctx = undefined;
    for (const check of this._def.checks) {
      if (check.kind === "min") {
        if (input.data.getTime() < check.value) {
          ctx = this._getOrReturnCtx(input, ctx);
          addIssueToContext(ctx, {
            code: ZodIssueCode.too_small,
            message: check.message,
            inclusive: true,
            exact: false,
            minimum: check.value,
            type: "date"
          });
          status.dirty();
        }
      } else if (check.kind === "max") {
        if (input.data.getTime() > check.value) {
          ctx = this._getOrReturnCtx(input, ctx);
          addIssueToContext(ctx, {
            code: ZodIssueCode.too_big,
            message: check.message,
            inclusive: true,
            exact: false,
            maximum: check.value,
            type: "date"
          });
          status.dirty();
        }
      } else {
        util.assertNever(check);
      }
    }
    return {
      status: status.value,
      value: new Date(input.data.getTime())
    };
  }
  _addCheck(check) {
    return new ZodDate({
      ...this._def,
      checks: [...this._def.checks, check]
    });
  }
  min(minDate, message) {
    return this._addCheck({
      kind: "min",
      value: minDate.getTime(),
      message: errorUtil.toString(message)
    });
  }
  max(maxDate, message) {
    return this._addCheck({
      kind: "max",
      value: maxDate.getTime(),
      message: errorUtil.toString(message)
    });
  }
  get minDate() {
    let min = null;
    for (const ch of this._def.checks) {
      if (ch.kind === "min") {
        if (min === null || ch.value > min)
          min = ch.value;
      }
    }
    return min != null ? new Date(min) : null;
  }
  get maxDate() {
    let max = null;
    for (const ch of this._def.checks) {
      if (ch.kind === "max") {
        if (max === null || ch.value < max)
          max = ch.value;
      }
    }
    return max != null ? new Date(max) : null;
  }
}
ZodDate.create = (params) => {
  return new ZodDate({
    checks: [],
    coerce: params?.coerce || false,
    typeName: ZodFirstPartyTypeKind.ZodDate,
    ...processCreateParams(params)
  });
};

class ZodSymbol extends ZodType {
  _parse(input) {
    const parsedType = this._getType(input);
    if (parsedType !== ZodParsedType.symbol) {
      const ctx = this._getOrReturnCtx(input);
      addIssueToContext(ctx, {
        code: ZodIssueCode.invalid_type,
        expected: ZodParsedType.symbol,
        received: ctx.parsedType
      });
      return INVALID;
    }
    return OK(input.data);
  }
}
ZodSymbol.create = (params) => {
  return new ZodSymbol({
    typeName: ZodFirstPartyTypeKind.ZodSymbol,
    ...processCreateParams(params)
  });
};

class ZodUndefined extends ZodType {
  _parse(input) {
    const parsedType = this._getType(input);
    if (parsedType !== ZodParsedType.undefined) {
      const ctx = this._getOrReturnCtx(input);
      addIssueToContext(ctx, {
        code: ZodIssueCode.invalid_type,
        expected: ZodParsedType.undefined,
        received: ctx.parsedType
      });
      return INVALID;
    }
    return OK(input.data);
  }
}
ZodUndefined.create = (params) => {
  return new ZodUndefined({
    typeName: ZodFirstPartyTypeKind.ZodUndefined,
    ...processCreateParams(params)
  });
};

class ZodNull extends ZodType {
  _parse(input) {
    const parsedType = this._getType(input);
    if (parsedType !== ZodParsedType.null) {
      const ctx = this._getOrReturnCtx(input);
      addIssueToContext(ctx, {
        code: ZodIssueCode.invalid_type,
        expected: ZodParsedType.null,
        received: ctx.parsedType
      });
      return INVALID;
    }
    return OK(input.data);
  }
}
ZodNull.create = (params) => {
  return new ZodNull({
    typeName: ZodFirstPartyTypeKind.ZodNull,
    ...processCreateParams(params)
  });
};

class ZodAny extends ZodType {
  constructor() {
    super(...arguments);
    this._any = true;
  }
  _parse(input) {
    return OK(input.data);
  }
}
ZodAny.create = (params) => {
  return new ZodAny({
    typeName: ZodFirstPartyTypeKind.ZodAny,
    ...processCreateParams(params)
  });
};

class ZodUnknown extends ZodType {
  constructor() {
    super(...arguments);
    this._unknown = true;
  }
  _parse(input) {
    return OK(input.data);
  }
}
ZodUnknown.create = (params) => {
  return new ZodUnknown({
    typeName: ZodFirstPartyTypeKind.ZodUnknown,
    ...processCreateParams(params)
  });
};

class ZodNever extends ZodType {
  _parse(input) {
    const ctx = this._getOrReturnCtx(input);
    addIssueToContext(ctx, {
      code: ZodIssueCode.invalid_type,
      expected: ZodParsedType.never,
      received: ctx.parsedType
    });
    return INVALID;
  }
}
ZodNever.create = (params) => {
  return new ZodNever({
    typeName: ZodFirstPartyTypeKind.ZodNever,
    ...processCreateParams(params)
  });
};

class ZodVoid extends ZodType {
  _parse(input) {
    const parsedType = this._getType(input);
    if (parsedType !== ZodParsedType.undefined) {
      const ctx = this._getOrReturnCtx(input);
      addIssueToContext(ctx, {
        code: ZodIssueCode.invalid_type,
        expected: ZodParsedType.void,
        received: ctx.parsedType
      });
      return INVALID;
    }
    return OK(input.data);
  }
}
ZodVoid.create = (params) => {
  return new ZodVoid({
    typeName: ZodFirstPartyTypeKind.ZodVoid,
    ...processCreateParams(params)
  });
};

class ZodArray extends ZodType {
  _parse(input) {
    const { ctx, status } = this._processInputParams(input);
    const def = this._def;
    if (ctx.parsedType !== ZodParsedType.array) {
      addIssueToContext(ctx, {
        code: ZodIssueCode.invalid_type,
        expected: ZodParsedType.array,
        received: ctx.parsedType
      });
      return INVALID;
    }
    if (def.exactLength !== null) {
      const tooBig = ctx.data.length > def.exactLength.value;
      const tooSmall = ctx.data.length < def.exactLength.value;
      if (tooBig || tooSmall) {
        addIssueToContext(ctx, {
          code: tooBig ? ZodIssueCode.too_big : ZodIssueCode.too_small,
          minimum: tooSmall ? def.exactLength.value : undefined,
          maximum: tooBig ? def.exactLength.value : undefined,
          type: "array",
          inclusive: true,
          exact: true,
          message: def.exactLength.message
        });
        status.dirty();
      }
    }
    if (def.minLength !== null) {
      if (ctx.data.length < def.minLength.value) {
        addIssueToContext(ctx, {
          code: ZodIssueCode.too_small,
          minimum: def.minLength.value,
          type: "array",
          inclusive: true,
          exact: false,
          message: def.minLength.message
        });
        status.dirty();
      }
    }
    if (def.maxLength !== null) {
      if (ctx.data.length > def.maxLength.value) {
        addIssueToContext(ctx, {
          code: ZodIssueCode.too_big,
          maximum: def.maxLength.value,
          type: "array",
          inclusive: true,
          exact: false,
          message: def.maxLength.message
        });
        status.dirty();
      }
    }
    if (ctx.common.async) {
      return Promise.all([...ctx.data].map((item, i) => {
        return def.type._parseAsync(new ParseInputLazyPath(ctx, item, ctx.path, i));
      })).then((result2) => {
        return ParseStatus.mergeArray(status, result2);
      });
    }
    const result = [...ctx.data].map((item, i) => {
      return def.type._parseSync(new ParseInputLazyPath(ctx, item, ctx.path, i));
    });
    return ParseStatus.mergeArray(status, result);
  }
  get element() {
    return this._def.type;
  }
  min(minLength, message) {
    return new ZodArray({
      ...this._def,
      minLength: { value: minLength, message: errorUtil.toString(message) }
    });
  }
  max(maxLength, message) {
    return new ZodArray({
      ...this._def,
      maxLength: { value: maxLength, message: errorUtil.toString(message) }
    });
  }
  length(len, message) {
    return new ZodArray({
      ...this._def,
      exactLength: { value: len, message: errorUtil.toString(message) }
    });
  }
  nonempty(message) {
    return this.min(1, message);
  }
}
ZodArray.create = (schema, params) => {
  return new ZodArray({
    type: schema,
    minLength: null,
    maxLength: null,
    exactLength: null,
    typeName: ZodFirstPartyTypeKind.ZodArray,
    ...processCreateParams(params)
  });
};
function deepPartialify(schema) {
  if (schema instanceof ZodObject) {
    const newShape = {};
    for (const key in schema.shape) {
      const fieldSchema = schema.shape[key];
      newShape[key] = ZodOptional.create(deepPartialify(fieldSchema));
    }
    return new ZodObject({
      ...schema._def,
      shape: () => newShape
    });
  } else if (schema instanceof ZodArray) {
    return new ZodArray({
      ...schema._def,
      type: deepPartialify(schema.element)
    });
  } else if (schema instanceof ZodOptional) {
    return ZodOptional.create(deepPartialify(schema.unwrap()));
  } else if (schema instanceof ZodNullable) {
    return ZodNullable.create(deepPartialify(schema.unwrap()));
  } else if (schema instanceof ZodTuple) {
    return ZodTuple.create(schema.items.map((item) => deepPartialify(item)));
  } else {
    return schema;
  }
}

class ZodObject extends ZodType {
  constructor() {
    super(...arguments);
    this._cached = null;
    this.nonstrict = this.passthrough;
    this.augment = this.extend;
  }
  _getCached() {
    if (this._cached !== null)
      return this._cached;
    const shape = this._def.shape();
    const keys = util.objectKeys(shape);
    this._cached = { shape, keys };
    return this._cached;
  }
  _parse(input) {
    const parsedType = this._getType(input);
    if (parsedType !== ZodParsedType.object) {
      const ctx2 = this._getOrReturnCtx(input);
      addIssueToContext(ctx2, {
        code: ZodIssueCode.invalid_type,
        expected: ZodParsedType.object,
        received: ctx2.parsedType
      });
      return INVALID;
    }
    const { status, ctx } = this._processInputParams(input);
    const { shape, keys: shapeKeys } = this._getCached();
    const extraKeys = [];
    if (!(this._def.catchall instanceof ZodNever && this._def.unknownKeys === "strip")) {
      for (const key in ctx.data) {
        if (!shapeKeys.includes(key)) {
          extraKeys.push(key);
        }
      }
    }
    const pairs = [];
    for (const key of shapeKeys) {
      const keyValidator = shape[key];
      const value = ctx.data[key];
      pairs.push({
        key: { status: "valid", value: key },
        value: keyValidator._parse(new ParseInputLazyPath(ctx, value, ctx.path, key)),
        alwaysSet: key in ctx.data
      });
    }
    if (this._def.catchall instanceof ZodNever) {
      const unknownKeys = this._def.unknownKeys;
      if (unknownKeys === "passthrough") {
        for (const key of extraKeys) {
          pairs.push({
            key: { status: "valid", value: key },
            value: { status: "valid", value: ctx.data[key] }
          });
        }
      } else if (unknownKeys === "strict") {
        if (extraKeys.length > 0) {
          addIssueToContext(ctx, {
            code: ZodIssueCode.unrecognized_keys,
            keys: extraKeys
          });
          status.dirty();
        }
      } else if (unknownKeys === "strip") {} else {
        throw new Error(`Internal ZodObject error: invalid unknownKeys value.`);
      }
    } else {
      const catchall = this._def.catchall;
      for (const key of extraKeys) {
        const value = ctx.data[key];
        pairs.push({
          key: { status: "valid", value: key },
          value: catchall._parse(new ParseInputLazyPath(ctx, value, ctx.path, key)),
          alwaysSet: key in ctx.data
        });
      }
    }
    if (ctx.common.async) {
      return Promise.resolve().then(async () => {
        const syncPairs = [];
        for (const pair of pairs) {
          const key = await pair.key;
          const value = await pair.value;
          syncPairs.push({
            key,
            value,
            alwaysSet: pair.alwaysSet
          });
        }
        return syncPairs;
      }).then((syncPairs) => {
        return ParseStatus.mergeObjectSync(status, syncPairs);
      });
    } else {
      return ParseStatus.mergeObjectSync(status, pairs);
    }
  }
  get shape() {
    return this._def.shape();
  }
  strict(message) {
    errorUtil.errToObj;
    return new ZodObject({
      ...this._def,
      unknownKeys: "strict",
      ...message !== undefined ? {
        errorMap: (issue, ctx) => {
          const defaultError = this._def.errorMap?.(issue, ctx).message ?? ctx.defaultError;
          if (issue.code === "unrecognized_keys")
            return {
              message: errorUtil.errToObj(message).message ?? defaultError
            };
          return {
            message: defaultError
          };
        }
      } : {}
    });
  }
  strip() {
    return new ZodObject({
      ...this._def,
      unknownKeys: "strip"
    });
  }
  passthrough() {
    return new ZodObject({
      ...this._def,
      unknownKeys: "passthrough"
    });
  }
  extend(augmentation) {
    return new ZodObject({
      ...this._def,
      shape: () => ({
        ...this._def.shape(),
        ...augmentation
      })
    });
  }
  merge(merging) {
    const merged = new ZodObject({
      unknownKeys: merging._def.unknownKeys,
      catchall: merging._def.catchall,
      shape: () => ({
        ...this._def.shape(),
        ...merging._def.shape()
      }),
      typeName: ZodFirstPartyTypeKind.ZodObject
    });
    return merged;
  }
  setKey(key, schema) {
    return this.augment({ [key]: schema });
  }
  catchall(index) {
    return new ZodObject({
      ...this._def,
      catchall: index
    });
  }
  pick(mask) {
    const shape = {};
    for (const key of util.objectKeys(mask)) {
      if (mask[key] && this.shape[key]) {
        shape[key] = this.shape[key];
      }
    }
    return new ZodObject({
      ...this._def,
      shape: () => shape
    });
  }
  omit(mask) {
    const shape = {};
    for (const key of util.objectKeys(this.shape)) {
      if (!mask[key]) {
        shape[key] = this.shape[key];
      }
    }
    return new ZodObject({
      ...this._def,
      shape: () => shape
    });
  }
  deepPartial() {
    return deepPartialify(this);
  }
  partial(mask) {
    const newShape = {};
    for (const key of util.objectKeys(this.shape)) {
      const fieldSchema = this.shape[key];
      if (mask && !mask[key]) {
        newShape[key] = fieldSchema;
      } else {
        newShape[key] = fieldSchema.optional();
      }
    }
    return new ZodObject({
      ...this._def,
      shape: () => newShape
    });
  }
  required(mask) {
    const newShape = {};
    for (const key of util.objectKeys(this.shape)) {
      if (mask && !mask[key]) {
        newShape[key] = this.shape[key];
      } else {
        const fieldSchema = this.shape[key];
        let newField = fieldSchema;
        while (newField instanceof ZodOptional) {
          newField = newField._def.innerType;
        }
        newShape[key] = newField;
      }
    }
    return new ZodObject({
      ...this._def,
      shape: () => newShape
    });
  }
  keyof() {
    return createZodEnum(util.objectKeys(this.shape));
  }
}
ZodObject.create = (shape, params) => {
  return new ZodObject({
    shape: () => shape,
    unknownKeys: "strip",
    catchall: ZodNever.create(),
    typeName: ZodFirstPartyTypeKind.ZodObject,
    ...processCreateParams(params)
  });
};
ZodObject.strictCreate = (shape, params) => {
  return new ZodObject({
    shape: () => shape,
    unknownKeys: "strict",
    catchall: ZodNever.create(),
    typeName: ZodFirstPartyTypeKind.ZodObject,
    ...processCreateParams(params)
  });
};
ZodObject.lazycreate = (shape, params) => {
  return new ZodObject({
    shape,
    unknownKeys: "strip",
    catchall: ZodNever.create(),
    typeName: ZodFirstPartyTypeKind.ZodObject,
    ...processCreateParams(params)
  });
};

class ZodUnion extends ZodType {
  _parse(input) {
    const { ctx } = this._processInputParams(input);
    const options = this._def.options;
    function handleResults(results) {
      for (const result of results) {
        if (result.result.status === "valid") {
          return result.result;
        }
      }
      for (const result of results) {
        if (result.result.status === "dirty") {
          ctx.common.issues.push(...result.ctx.common.issues);
          return result.result;
        }
      }
      const unionErrors = results.map((result) => new ZodError(result.ctx.common.issues));
      addIssueToContext(ctx, {
        code: ZodIssueCode.invalid_union,
        unionErrors
      });
      return INVALID;
    }
    if (ctx.common.async) {
      return Promise.all(options.map(async (option) => {
        const childCtx = {
          ...ctx,
          common: {
            ...ctx.common,
            issues: []
          },
          parent: null
        };
        return {
          result: await option._parseAsync({
            data: ctx.data,
            path: ctx.path,
            parent: childCtx
          }),
          ctx: childCtx
        };
      })).then(handleResults);
    } else {
      let dirty = undefined;
      const issues = [];
      for (const option of options) {
        const childCtx = {
          ...ctx,
          common: {
            ...ctx.common,
            issues: []
          },
          parent: null
        };
        const result = option._parseSync({
          data: ctx.data,
          path: ctx.path,
          parent: childCtx
        });
        if (result.status === "valid") {
          return result;
        } else if (result.status === "dirty" && !dirty) {
          dirty = { result, ctx: childCtx };
        }
        if (childCtx.common.issues.length) {
          issues.push(childCtx.common.issues);
        }
      }
      if (dirty) {
        ctx.common.issues.push(...dirty.ctx.common.issues);
        return dirty.result;
      }
      const unionErrors = issues.map((issues2) => new ZodError(issues2));
      addIssueToContext(ctx, {
        code: ZodIssueCode.invalid_union,
        unionErrors
      });
      return INVALID;
    }
  }
  get options() {
    return this._def.options;
  }
}
ZodUnion.create = (types, params) => {
  return new ZodUnion({
    options: types,
    typeName: ZodFirstPartyTypeKind.ZodUnion,
    ...processCreateParams(params)
  });
};
var getDiscriminator = (type) => {
  if (type instanceof ZodLazy) {
    return getDiscriminator(type.schema);
  } else if (type instanceof ZodEffects) {
    return getDiscriminator(type.innerType());
  } else if (type instanceof ZodLiteral) {
    return [type.value];
  } else if (type instanceof ZodEnum) {
    return type.options;
  } else if (type instanceof ZodNativeEnum) {
    return util.objectValues(type.enum);
  } else if (type instanceof ZodDefault) {
    return getDiscriminator(type._def.innerType);
  } else if (type instanceof ZodUndefined) {
    return [undefined];
  } else if (type instanceof ZodNull) {
    return [null];
  } else if (type instanceof ZodOptional) {
    return [undefined, ...getDiscriminator(type.unwrap())];
  } else if (type instanceof ZodNullable) {
    return [null, ...getDiscriminator(type.unwrap())];
  } else if (type instanceof ZodBranded) {
    return getDiscriminator(type.unwrap());
  } else if (type instanceof ZodReadonly) {
    return getDiscriminator(type.unwrap());
  } else if (type instanceof ZodCatch) {
    return getDiscriminator(type._def.innerType);
  } else {
    return [];
  }
};

class ZodDiscriminatedUnion extends ZodType {
  _parse(input) {
    const { ctx } = this._processInputParams(input);
    if (ctx.parsedType !== ZodParsedType.object) {
      addIssueToContext(ctx, {
        code: ZodIssueCode.invalid_type,
        expected: ZodParsedType.object,
        received: ctx.parsedType
      });
      return INVALID;
    }
    const discriminator = this.discriminator;
    const discriminatorValue = ctx.data[discriminator];
    const option = this.optionsMap.get(discriminatorValue);
    if (!option) {
      addIssueToContext(ctx, {
        code: ZodIssueCode.invalid_union_discriminator,
        options: Array.from(this.optionsMap.keys()),
        path: [discriminator]
      });
      return INVALID;
    }
    if (ctx.common.async) {
      return option._parseAsync({
        data: ctx.data,
        path: ctx.path,
        parent: ctx
      });
    } else {
      return option._parseSync({
        data: ctx.data,
        path: ctx.path,
        parent: ctx
      });
    }
  }
  get discriminator() {
    return this._def.discriminator;
  }
  get options() {
    return this._def.options;
  }
  get optionsMap() {
    return this._def.optionsMap;
  }
  static create(discriminator, options, params) {
    const optionsMap = new Map;
    for (const type of options) {
      const discriminatorValues = getDiscriminator(type.shape[discriminator]);
      if (!discriminatorValues.length) {
        throw new Error(`A discriminator value for key \`${discriminator}\` could not be extracted from all schema options`);
      }
      for (const value of discriminatorValues) {
        if (optionsMap.has(value)) {
          throw new Error(`Discriminator property ${String(discriminator)} has duplicate value ${String(value)}`);
        }
        optionsMap.set(value, type);
      }
    }
    return new ZodDiscriminatedUnion({
      typeName: ZodFirstPartyTypeKind.ZodDiscriminatedUnion,
      discriminator,
      options,
      optionsMap,
      ...processCreateParams(params)
    });
  }
}
function mergeValues(a, b) {
  const aType = getParsedType(a);
  const bType = getParsedType(b);
  if (a === b) {
    return { valid: true, data: a };
  } else if (aType === ZodParsedType.object && bType === ZodParsedType.object) {
    const bKeys = util.objectKeys(b);
    const sharedKeys = util.objectKeys(a).filter((key) => bKeys.indexOf(key) !== -1);
    const newObj = { ...a, ...b };
    for (const key of sharedKeys) {
      const sharedValue = mergeValues(a[key], b[key]);
      if (!sharedValue.valid) {
        return { valid: false };
      }
      newObj[key] = sharedValue.data;
    }
    return { valid: true, data: newObj };
  } else if (aType === ZodParsedType.array && bType === ZodParsedType.array) {
    if (a.length !== b.length) {
      return { valid: false };
    }
    const newArray = [];
    for (let index = 0;index < a.length; index++) {
      const itemA = a[index];
      const itemB = b[index];
      const sharedValue = mergeValues(itemA, itemB);
      if (!sharedValue.valid) {
        return { valid: false };
      }
      newArray.push(sharedValue.data);
    }
    return { valid: true, data: newArray };
  } else if (aType === ZodParsedType.date && bType === ZodParsedType.date && +a === +b) {
    return { valid: true, data: a };
  } else {
    return { valid: false };
  }
}

class ZodIntersection extends ZodType {
  _parse(input) {
    const { status, ctx } = this._processInputParams(input);
    const handleParsed = (parsedLeft, parsedRight) => {
      if (isAborted(parsedLeft) || isAborted(parsedRight)) {
        return INVALID;
      }
      const merged = mergeValues(parsedLeft.value, parsedRight.value);
      if (!merged.valid) {
        addIssueToContext(ctx, {
          code: ZodIssueCode.invalid_intersection_types
        });
        return INVALID;
      }
      if (isDirty(parsedLeft) || isDirty(parsedRight)) {
        status.dirty();
      }
      return { status: status.value, value: merged.data };
    };
    if (ctx.common.async) {
      return Promise.all([
        this._def.left._parseAsync({
          data: ctx.data,
          path: ctx.path,
          parent: ctx
        }),
        this._def.right._parseAsync({
          data: ctx.data,
          path: ctx.path,
          parent: ctx
        })
      ]).then(([left, right]) => handleParsed(left, right));
    } else {
      return handleParsed(this._def.left._parseSync({
        data: ctx.data,
        path: ctx.path,
        parent: ctx
      }), this._def.right._parseSync({
        data: ctx.data,
        path: ctx.path,
        parent: ctx
      }));
    }
  }
}
ZodIntersection.create = (left, right, params) => {
  return new ZodIntersection({
    left,
    right,
    typeName: ZodFirstPartyTypeKind.ZodIntersection,
    ...processCreateParams(params)
  });
};

class ZodTuple extends ZodType {
  _parse(input) {
    const { status, ctx } = this._processInputParams(input);
    if (ctx.parsedType !== ZodParsedType.array) {
      addIssueToContext(ctx, {
        code: ZodIssueCode.invalid_type,
        expected: ZodParsedType.array,
        received: ctx.parsedType
      });
      return INVALID;
    }
    if (ctx.data.length < this._def.items.length) {
      addIssueToContext(ctx, {
        code: ZodIssueCode.too_small,
        minimum: this._def.items.length,
        inclusive: true,
        exact: false,
        type: "array"
      });
      return INVALID;
    }
    const rest = this._def.rest;
    if (!rest && ctx.data.length > this._def.items.length) {
      addIssueToContext(ctx, {
        code: ZodIssueCode.too_big,
        maximum: this._def.items.length,
        inclusive: true,
        exact: false,
        type: "array"
      });
      status.dirty();
    }
    const items = [...ctx.data].map((item, itemIndex) => {
      const schema = this._def.items[itemIndex] || this._def.rest;
      if (!schema)
        return null;
      return schema._parse(new ParseInputLazyPath(ctx, item, ctx.path, itemIndex));
    }).filter((x) => !!x);
    if (ctx.common.async) {
      return Promise.all(items).then((results) => {
        return ParseStatus.mergeArray(status, results);
      });
    } else {
      return ParseStatus.mergeArray(status, items);
    }
  }
  get items() {
    return this._def.items;
  }
  rest(rest) {
    return new ZodTuple({
      ...this._def,
      rest
    });
  }
}
ZodTuple.create = (schemas, params) => {
  if (!Array.isArray(schemas)) {
    throw new Error("You must pass an array of schemas to z.tuple([ ... ])");
  }
  return new ZodTuple({
    items: schemas,
    typeName: ZodFirstPartyTypeKind.ZodTuple,
    rest: null,
    ...processCreateParams(params)
  });
};

class ZodRecord extends ZodType {
  get keySchema() {
    return this._def.keyType;
  }
  get valueSchema() {
    return this._def.valueType;
  }
  _parse(input) {
    const { status, ctx } = this._processInputParams(input);
    if (ctx.parsedType !== ZodParsedType.object) {
      addIssueToContext(ctx, {
        code: ZodIssueCode.invalid_type,
        expected: ZodParsedType.object,
        received: ctx.parsedType
      });
      return INVALID;
    }
    const pairs = [];
    const keyType = this._def.keyType;
    const valueType = this._def.valueType;
    for (const key in ctx.data) {
      pairs.push({
        key: keyType._parse(new ParseInputLazyPath(ctx, key, ctx.path, key)),
        value: valueType._parse(new ParseInputLazyPath(ctx, ctx.data[key], ctx.path, key)),
        alwaysSet: key in ctx.data
      });
    }
    if (ctx.common.async) {
      return ParseStatus.mergeObjectAsync(status, pairs);
    } else {
      return ParseStatus.mergeObjectSync(status, pairs);
    }
  }
  get element() {
    return this._def.valueType;
  }
  static create(first, second, third) {
    if (second instanceof ZodType) {
      return new ZodRecord({
        keyType: first,
        valueType: second,
        typeName: ZodFirstPartyTypeKind.ZodRecord,
        ...processCreateParams(third)
      });
    }
    return new ZodRecord({
      keyType: ZodString.create(),
      valueType: first,
      typeName: ZodFirstPartyTypeKind.ZodRecord,
      ...processCreateParams(second)
    });
  }
}

class ZodMap extends ZodType {
  get keySchema() {
    return this._def.keyType;
  }
  get valueSchema() {
    return this._def.valueType;
  }
  _parse(input) {
    const { status, ctx } = this._processInputParams(input);
    if (ctx.parsedType !== ZodParsedType.map) {
      addIssueToContext(ctx, {
        code: ZodIssueCode.invalid_type,
        expected: ZodParsedType.map,
        received: ctx.parsedType
      });
      return INVALID;
    }
    const keyType = this._def.keyType;
    const valueType = this._def.valueType;
    const pairs = [...ctx.data.entries()].map(([key, value], index) => {
      return {
        key: keyType._parse(new ParseInputLazyPath(ctx, key, ctx.path, [index, "key"])),
        value: valueType._parse(new ParseInputLazyPath(ctx, value, ctx.path, [index, "value"]))
      };
    });
    if (ctx.common.async) {
      const finalMap = new Map;
      return Promise.resolve().then(async () => {
        for (const pair of pairs) {
          const key = await pair.key;
          const value = await pair.value;
          if (key.status === "aborted" || value.status === "aborted") {
            return INVALID;
          }
          if (key.status === "dirty" || value.status === "dirty") {
            status.dirty();
          }
          finalMap.set(key.value, value.value);
        }
        return { status: status.value, value: finalMap };
      });
    } else {
      const finalMap = new Map;
      for (const pair of pairs) {
        const key = pair.key;
        const value = pair.value;
        if (key.status === "aborted" || value.status === "aborted") {
          return INVALID;
        }
        if (key.status === "dirty" || value.status === "dirty") {
          status.dirty();
        }
        finalMap.set(key.value, value.value);
      }
      return { status: status.value, value: finalMap };
    }
  }
}
ZodMap.create = (keyType, valueType, params) => {
  return new ZodMap({
    valueType,
    keyType,
    typeName: ZodFirstPartyTypeKind.ZodMap,
    ...processCreateParams(params)
  });
};

class ZodSet extends ZodType {
  _parse(input) {
    const { status, ctx } = this._processInputParams(input);
    if (ctx.parsedType !== ZodParsedType.set) {
      addIssueToContext(ctx, {
        code: ZodIssueCode.invalid_type,
        expected: ZodParsedType.set,
        received: ctx.parsedType
      });
      return INVALID;
    }
    const def = this._def;
    if (def.minSize !== null) {
      if (ctx.data.size < def.minSize.value) {
        addIssueToContext(ctx, {
          code: ZodIssueCode.too_small,
          minimum: def.minSize.value,
          type: "set",
          inclusive: true,
          exact: false,
          message: def.minSize.message
        });
        status.dirty();
      }
    }
    if (def.maxSize !== null) {
      if (ctx.data.size > def.maxSize.value) {
        addIssueToContext(ctx, {
          code: ZodIssueCode.too_big,
          maximum: def.maxSize.value,
          type: "set",
          inclusive: true,
          exact: false,
          message: def.maxSize.message
        });
        status.dirty();
      }
    }
    const valueType = this._def.valueType;
    function finalizeSet(elements2) {
      const parsedSet = new Set;
      for (const element of elements2) {
        if (element.status === "aborted")
          return INVALID;
        if (element.status === "dirty")
          status.dirty();
        parsedSet.add(element.value);
      }
      return { status: status.value, value: parsedSet };
    }
    const elements = [...ctx.data.values()].map((item, i) => valueType._parse(new ParseInputLazyPath(ctx, item, ctx.path, i)));
    if (ctx.common.async) {
      return Promise.all(elements).then((elements2) => finalizeSet(elements2));
    } else {
      return finalizeSet(elements);
    }
  }
  min(minSize, message) {
    return new ZodSet({
      ...this._def,
      minSize: { value: minSize, message: errorUtil.toString(message) }
    });
  }
  max(maxSize, message) {
    return new ZodSet({
      ...this._def,
      maxSize: { value: maxSize, message: errorUtil.toString(message) }
    });
  }
  size(size, message) {
    return this.min(size, message).max(size, message);
  }
  nonempty(message) {
    return this.min(1, message);
  }
}
ZodSet.create = (valueType, params) => {
  return new ZodSet({
    valueType,
    minSize: null,
    maxSize: null,
    typeName: ZodFirstPartyTypeKind.ZodSet,
    ...processCreateParams(params)
  });
};

class ZodFunction extends ZodType {
  constructor() {
    super(...arguments);
    this.validate = this.implement;
  }
  _parse(input) {
    const { ctx } = this._processInputParams(input);
    if (ctx.parsedType !== ZodParsedType.function) {
      addIssueToContext(ctx, {
        code: ZodIssueCode.invalid_type,
        expected: ZodParsedType.function,
        received: ctx.parsedType
      });
      return INVALID;
    }
    function makeArgsIssue(args, error) {
      return makeIssue({
        data: args,
        path: ctx.path,
        errorMaps: [ctx.common.contextualErrorMap, ctx.schemaErrorMap, getErrorMap(), en_default].filter((x) => !!x),
        issueData: {
          code: ZodIssueCode.invalid_arguments,
          argumentsError: error
        }
      });
    }
    function makeReturnsIssue(returns, error) {
      return makeIssue({
        data: returns,
        path: ctx.path,
        errorMaps: [ctx.common.contextualErrorMap, ctx.schemaErrorMap, getErrorMap(), en_default].filter((x) => !!x),
        issueData: {
          code: ZodIssueCode.invalid_return_type,
          returnTypeError: error
        }
      });
    }
    const params = { errorMap: ctx.common.contextualErrorMap };
    const fn = ctx.data;
    if (this._def.returns instanceof ZodPromise) {
      const me = this;
      return OK(async function(...args) {
        const error = new ZodError([]);
        const parsedArgs = await me._def.args.parseAsync(args, params).catch((e) => {
          error.addIssue(makeArgsIssue(args, e));
          throw error;
        });
        const result = await Reflect.apply(fn, this, parsedArgs);
        const parsedReturns = await me._def.returns._def.type.parseAsync(result, params).catch((e) => {
          error.addIssue(makeReturnsIssue(result, e));
          throw error;
        });
        return parsedReturns;
      });
    } else {
      const me = this;
      return OK(function(...args) {
        const parsedArgs = me._def.args.safeParse(args, params);
        if (!parsedArgs.success) {
          throw new ZodError([makeArgsIssue(args, parsedArgs.error)]);
        }
        const result = Reflect.apply(fn, this, parsedArgs.data);
        const parsedReturns = me._def.returns.safeParse(result, params);
        if (!parsedReturns.success) {
          throw new ZodError([makeReturnsIssue(result, parsedReturns.error)]);
        }
        return parsedReturns.data;
      });
    }
  }
  parameters() {
    return this._def.args;
  }
  returnType() {
    return this._def.returns;
  }
  args(...items) {
    return new ZodFunction({
      ...this._def,
      args: ZodTuple.create(items).rest(ZodUnknown.create())
    });
  }
  returns(returnType) {
    return new ZodFunction({
      ...this._def,
      returns: returnType
    });
  }
  implement(func) {
    const validatedFunc = this.parse(func);
    return validatedFunc;
  }
  strictImplement(func) {
    const validatedFunc = this.parse(func);
    return validatedFunc;
  }
  static create(args, returns, params) {
    return new ZodFunction({
      args: args ? args : ZodTuple.create([]).rest(ZodUnknown.create()),
      returns: returns || ZodUnknown.create(),
      typeName: ZodFirstPartyTypeKind.ZodFunction,
      ...processCreateParams(params)
    });
  }
}

class ZodLazy extends ZodType {
  get schema() {
    return this._def.getter();
  }
  _parse(input) {
    const { ctx } = this._processInputParams(input);
    const lazySchema = this._def.getter();
    return lazySchema._parse({ data: ctx.data, path: ctx.path, parent: ctx });
  }
}
ZodLazy.create = (getter, params) => {
  return new ZodLazy({
    getter,
    typeName: ZodFirstPartyTypeKind.ZodLazy,
    ...processCreateParams(params)
  });
};

class ZodLiteral extends ZodType {
  _parse(input) {
    if (input.data !== this._def.value) {
      const ctx = this._getOrReturnCtx(input);
      addIssueToContext(ctx, {
        received: ctx.data,
        code: ZodIssueCode.invalid_literal,
        expected: this._def.value
      });
      return INVALID;
    }
    return { status: "valid", value: input.data };
  }
  get value() {
    return this._def.value;
  }
}
ZodLiteral.create = (value, params) => {
  return new ZodLiteral({
    value,
    typeName: ZodFirstPartyTypeKind.ZodLiteral,
    ...processCreateParams(params)
  });
};
function createZodEnum(values, params) {
  return new ZodEnum({
    values,
    typeName: ZodFirstPartyTypeKind.ZodEnum,
    ...processCreateParams(params)
  });
}

class ZodEnum extends ZodType {
  _parse(input) {
    if (typeof input.data !== "string") {
      const ctx = this._getOrReturnCtx(input);
      const expectedValues = this._def.values;
      addIssueToContext(ctx, {
        expected: util.joinValues(expectedValues),
        received: ctx.parsedType,
        code: ZodIssueCode.invalid_type
      });
      return INVALID;
    }
    if (!this._cache) {
      this._cache = new Set(this._def.values);
    }
    if (!this._cache.has(input.data)) {
      const ctx = this._getOrReturnCtx(input);
      const expectedValues = this._def.values;
      addIssueToContext(ctx, {
        received: ctx.data,
        code: ZodIssueCode.invalid_enum_value,
        options: expectedValues
      });
      return INVALID;
    }
    return OK(input.data);
  }
  get options() {
    return this._def.values;
  }
  get enum() {
    const enumValues = {};
    for (const val of this._def.values) {
      enumValues[val] = val;
    }
    return enumValues;
  }
  get Values() {
    const enumValues = {};
    for (const val of this._def.values) {
      enumValues[val] = val;
    }
    return enumValues;
  }
  get Enum() {
    const enumValues = {};
    for (const val of this._def.values) {
      enumValues[val] = val;
    }
    return enumValues;
  }
  extract(values, newDef = this._def) {
    return ZodEnum.create(values, {
      ...this._def,
      ...newDef
    });
  }
  exclude(values, newDef = this._def) {
    return ZodEnum.create(this.options.filter((opt) => !values.includes(opt)), {
      ...this._def,
      ...newDef
    });
  }
}
ZodEnum.create = createZodEnum;

class ZodNativeEnum extends ZodType {
  _parse(input) {
    const nativeEnumValues = util.getValidEnumValues(this._def.values);
    const ctx = this._getOrReturnCtx(input);
    if (ctx.parsedType !== ZodParsedType.string && ctx.parsedType !== ZodParsedType.number) {
      const expectedValues = util.objectValues(nativeEnumValues);
      addIssueToContext(ctx, {
        expected: util.joinValues(expectedValues),
        received: ctx.parsedType,
        code: ZodIssueCode.invalid_type
      });
      return INVALID;
    }
    if (!this._cache) {
      this._cache = new Set(util.getValidEnumValues(this._def.values));
    }
    if (!this._cache.has(input.data)) {
      const expectedValues = util.objectValues(nativeEnumValues);
      addIssueToContext(ctx, {
        received: ctx.data,
        code: ZodIssueCode.invalid_enum_value,
        options: expectedValues
      });
      return INVALID;
    }
    return OK(input.data);
  }
  get enum() {
    return this._def.values;
  }
}
ZodNativeEnum.create = (values, params) => {
  return new ZodNativeEnum({
    values,
    typeName: ZodFirstPartyTypeKind.ZodNativeEnum,
    ...processCreateParams(params)
  });
};

class ZodPromise extends ZodType {
  unwrap() {
    return this._def.type;
  }
  _parse(input) {
    const { ctx } = this._processInputParams(input);
    if (ctx.parsedType !== ZodParsedType.promise && ctx.common.async === false) {
      addIssueToContext(ctx, {
        code: ZodIssueCode.invalid_type,
        expected: ZodParsedType.promise,
        received: ctx.parsedType
      });
      return INVALID;
    }
    const promisified = ctx.parsedType === ZodParsedType.promise ? ctx.data : Promise.resolve(ctx.data);
    return OK(promisified.then((data) => {
      return this._def.type.parseAsync(data, {
        path: ctx.path,
        errorMap: ctx.common.contextualErrorMap
      });
    }));
  }
}
ZodPromise.create = (schema, params) => {
  return new ZodPromise({
    type: schema,
    typeName: ZodFirstPartyTypeKind.ZodPromise,
    ...processCreateParams(params)
  });
};

class ZodEffects extends ZodType {
  innerType() {
    return this._def.schema;
  }
  sourceType() {
    return this._def.schema._def.typeName === ZodFirstPartyTypeKind.ZodEffects ? this._def.schema.sourceType() : this._def.schema;
  }
  _parse(input) {
    const { status, ctx } = this._processInputParams(input);
    const effect = this._def.effect || null;
    const checkCtx = {
      addIssue: (arg) => {
        addIssueToContext(ctx, arg);
        if (arg.fatal) {
          status.abort();
        } else {
          status.dirty();
        }
      },
      get path() {
        return ctx.path;
      }
    };
    checkCtx.addIssue = checkCtx.addIssue.bind(checkCtx);
    if (effect.type === "preprocess") {
      const processed = effect.transform(ctx.data, checkCtx);
      if (ctx.common.async) {
        return Promise.resolve(processed).then(async (processed2) => {
          if (status.value === "aborted")
            return INVALID;
          const result = await this._def.schema._parseAsync({
            data: processed2,
            path: ctx.path,
            parent: ctx
          });
          if (result.status === "aborted")
            return INVALID;
          if (result.status === "dirty")
            return DIRTY(result.value);
          if (status.value === "dirty")
            return DIRTY(result.value);
          return result;
        });
      } else {
        if (status.value === "aborted")
          return INVALID;
        const result = this._def.schema._parseSync({
          data: processed,
          path: ctx.path,
          parent: ctx
        });
        if (result.status === "aborted")
          return INVALID;
        if (result.status === "dirty")
          return DIRTY(result.value);
        if (status.value === "dirty")
          return DIRTY(result.value);
        return result;
      }
    }
    if (effect.type === "refinement") {
      const executeRefinement = (acc) => {
        const result = effect.refinement(acc, checkCtx);
        if (ctx.common.async) {
          return Promise.resolve(result);
        }
        if (result instanceof Promise) {
          throw new Error("Async refinement encountered during synchronous parse operation. Use .parseAsync instead.");
        }
        return acc;
      };
      if (ctx.common.async === false) {
        const inner = this._def.schema._parseSync({
          data: ctx.data,
          path: ctx.path,
          parent: ctx
        });
        if (inner.status === "aborted")
          return INVALID;
        if (inner.status === "dirty")
          status.dirty();
        executeRefinement(inner.value);
        return { status: status.value, value: inner.value };
      } else {
        return this._def.schema._parseAsync({ data: ctx.data, path: ctx.path, parent: ctx }).then((inner) => {
          if (inner.status === "aborted")
            return INVALID;
          if (inner.status === "dirty")
            status.dirty();
          return executeRefinement(inner.value).then(() => {
            return { status: status.value, value: inner.value };
          });
        });
      }
    }
    if (effect.type === "transform") {
      if (ctx.common.async === false) {
        const base = this._def.schema._parseSync({
          data: ctx.data,
          path: ctx.path,
          parent: ctx
        });
        if (!isValid(base))
          return INVALID;
        const result = effect.transform(base.value, checkCtx);
        if (result instanceof Promise) {
          throw new Error(`Asynchronous transform encountered during synchronous parse operation. Use .parseAsync instead.`);
        }
        return { status: status.value, value: result };
      } else {
        return this._def.schema._parseAsync({ data: ctx.data, path: ctx.path, parent: ctx }).then((base) => {
          if (!isValid(base))
            return INVALID;
          return Promise.resolve(effect.transform(base.value, checkCtx)).then((result) => ({
            status: status.value,
            value: result
          }));
        });
      }
    }
    util.assertNever(effect);
  }
}
ZodEffects.create = (schema, effect, params) => {
  return new ZodEffects({
    schema,
    typeName: ZodFirstPartyTypeKind.ZodEffects,
    effect,
    ...processCreateParams(params)
  });
};
ZodEffects.createWithPreprocess = (preprocess, schema, params) => {
  return new ZodEffects({
    schema,
    effect: { type: "preprocess", transform: preprocess },
    typeName: ZodFirstPartyTypeKind.ZodEffects,
    ...processCreateParams(params)
  });
};
class ZodOptional extends ZodType {
  _parse(input) {
    const parsedType = this._getType(input);
    if (parsedType === ZodParsedType.undefined) {
      return OK(undefined);
    }
    return this._def.innerType._parse(input);
  }
  unwrap() {
    return this._def.innerType;
  }
}
ZodOptional.create = (type, params) => {
  return new ZodOptional({
    innerType: type,
    typeName: ZodFirstPartyTypeKind.ZodOptional,
    ...processCreateParams(params)
  });
};

class ZodNullable extends ZodType {
  _parse(input) {
    const parsedType = this._getType(input);
    if (parsedType === ZodParsedType.null) {
      return OK(null);
    }
    return this._def.innerType._parse(input);
  }
  unwrap() {
    return this._def.innerType;
  }
}
ZodNullable.create = (type, params) => {
  return new ZodNullable({
    innerType: type,
    typeName: ZodFirstPartyTypeKind.ZodNullable,
    ...processCreateParams(params)
  });
};

class ZodDefault extends ZodType {
  _parse(input) {
    const { ctx } = this._processInputParams(input);
    let data = ctx.data;
    if (ctx.parsedType === ZodParsedType.undefined) {
      data = this._def.defaultValue();
    }
    return this._def.innerType._parse({
      data,
      path: ctx.path,
      parent: ctx
    });
  }
  removeDefault() {
    return this._def.innerType;
  }
}
ZodDefault.create = (type, params) => {
  return new ZodDefault({
    innerType: type,
    typeName: ZodFirstPartyTypeKind.ZodDefault,
    defaultValue: typeof params.default === "function" ? params.default : () => params.default,
    ...processCreateParams(params)
  });
};

class ZodCatch extends ZodType {
  _parse(input) {
    const { ctx } = this._processInputParams(input);
    const newCtx = {
      ...ctx,
      common: {
        ...ctx.common,
        issues: []
      }
    };
    const result = this._def.innerType._parse({
      data: newCtx.data,
      path: newCtx.path,
      parent: {
        ...newCtx
      }
    });
    if (isAsync(result)) {
      return result.then((result2) => {
        return {
          status: "valid",
          value: result2.status === "valid" ? result2.value : this._def.catchValue({
            get error() {
              return new ZodError(newCtx.common.issues);
            },
            input: newCtx.data
          })
        };
      });
    } else {
      return {
        status: "valid",
        value: result.status === "valid" ? result.value : this._def.catchValue({
          get error() {
            return new ZodError(newCtx.common.issues);
          },
          input: newCtx.data
        })
      };
    }
  }
  removeCatch() {
    return this._def.innerType;
  }
}
ZodCatch.create = (type, params) => {
  return new ZodCatch({
    innerType: type,
    typeName: ZodFirstPartyTypeKind.ZodCatch,
    catchValue: typeof params.catch === "function" ? params.catch : () => params.catch,
    ...processCreateParams(params)
  });
};

class ZodNaN extends ZodType {
  _parse(input) {
    const parsedType = this._getType(input);
    if (parsedType !== ZodParsedType.nan) {
      const ctx = this._getOrReturnCtx(input);
      addIssueToContext(ctx, {
        code: ZodIssueCode.invalid_type,
        expected: ZodParsedType.nan,
        received: ctx.parsedType
      });
      return INVALID;
    }
    return { status: "valid", value: input.data };
  }
}
ZodNaN.create = (params) => {
  return new ZodNaN({
    typeName: ZodFirstPartyTypeKind.ZodNaN,
    ...processCreateParams(params)
  });
};
var BRAND = Symbol("zod_brand");

class ZodBranded extends ZodType {
  _parse(input) {
    const { ctx } = this._processInputParams(input);
    const data = ctx.data;
    return this._def.type._parse({
      data,
      path: ctx.path,
      parent: ctx
    });
  }
  unwrap() {
    return this._def.type;
  }
}

class ZodPipeline extends ZodType {
  _parse(input) {
    const { status, ctx } = this._processInputParams(input);
    if (ctx.common.async) {
      const handleAsync = async () => {
        const inResult = await this._def.in._parseAsync({
          data: ctx.data,
          path: ctx.path,
          parent: ctx
        });
        if (inResult.status === "aborted")
          return INVALID;
        if (inResult.status === "dirty") {
          status.dirty();
          return DIRTY(inResult.value);
        } else {
          return this._def.out._parseAsync({
            data: inResult.value,
            path: ctx.path,
            parent: ctx
          });
        }
      };
      return handleAsync();
    } else {
      const inResult = this._def.in._parseSync({
        data: ctx.data,
        path: ctx.path,
        parent: ctx
      });
      if (inResult.status === "aborted")
        return INVALID;
      if (inResult.status === "dirty") {
        status.dirty();
        return {
          status: "dirty",
          value: inResult.value
        };
      } else {
        return this._def.out._parseSync({
          data: inResult.value,
          path: ctx.path,
          parent: ctx
        });
      }
    }
  }
  static create(a, b) {
    return new ZodPipeline({
      in: a,
      out: b,
      typeName: ZodFirstPartyTypeKind.ZodPipeline
    });
  }
}

class ZodReadonly extends ZodType {
  _parse(input) {
    const result = this._def.innerType._parse(input);
    const freeze = (data) => {
      if (isValid(data)) {
        data.value = Object.freeze(data.value);
      }
      return data;
    };
    return isAsync(result) ? result.then((data) => freeze(data)) : freeze(result);
  }
  unwrap() {
    return this._def.innerType;
  }
}
ZodReadonly.create = (type, params) => {
  return new ZodReadonly({
    innerType: type,
    typeName: ZodFirstPartyTypeKind.ZodReadonly,
    ...processCreateParams(params)
  });
};
function cleanParams(params, data) {
  const p = typeof params === "function" ? params(data) : typeof params === "string" ? { message: params } : params;
  const p2 = typeof p === "string" ? { message: p } : p;
  return p2;
}
function custom(check, _params = {}, fatal) {
  if (check)
    return ZodAny.create().superRefine((data, ctx) => {
      const r = check(data);
      if (r instanceof Promise) {
        return r.then((r2) => {
          if (!r2) {
            const params = cleanParams(_params, data);
            const _fatal = params.fatal ?? fatal ?? true;
            ctx.addIssue({ code: "custom", ...params, fatal: _fatal });
          }
        });
      }
      if (!r) {
        const params = cleanParams(_params, data);
        const _fatal = params.fatal ?? fatal ?? true;
        ctx.addIssue({ code: "custom", ...params, fatal: _fatal });
      }
      return;
    });
  return ZodAny.create();
}
var late = {
  object: ZodObject.lazycreate
};
var ZodFirstPartyTypeKind;
(function(ZodFirstPartyTypeKind2) {
  ZodFirstPartyTypeKind2["ZodString"] = "ZodString";
  ZodFirstPartyTypeKind2["ZodNumber"] = "ZodNumber";
  ZodFirstPartyTypeKind2["ZodNaN"] = "ZodNaN";
  ZodFirstPartyTypeKind2["ZodBigInt"] = "ZodBigInt";
  ZodFirstPartyTypeKind2["ZodBoolean"] = "ZodBoolean";
  ZodFirstPartyTypeKind2["ZodDate"] = "ZodDate";
  ZodFirstPartyTypeKind2["ZodSymbol"] = "ZodSymbol";
  ZodFirstPartyTypeKind2["ZodUndefined"] = "ZodUndefined";
  ZodFirstPartyTypeKind2["ZodNull"] = "ZodNull";
  ZodFirstPartyTypeKind2["ZodAny"] = "ZodAny";
  ZodFirstPartyTypeKind2["ZodUnknown"] = "ZodUnknown";
  ZodFirstPartyTypeKind2["ZodNever"] = "ZodNever";
  ZodFirstPartyTypeKind2["ZodVoid"] = "ZodVoid";
  ZodFirstPartyTypeKind2["ZodArray"] = "ZodArray";
  ZodFirstPartyTypeKind2["ZodObject"] = "ZodObject";
  ZodFirstPartyTypeKind2["ZodUnion"] = "ZodUnion";
  ZodFirstPartyTypeKind2["ZodDiscriminatedUnion"] = "ZodDiscriminatedUnion";
  ZodFirstPartyTypeKind2["ZodIntersection"] = "ZodIntersection";
  ZodFirstPartyTypeKind2["ZodTuple"] = "ZodTuple";
  ZodFirstPartyTypeKind2["ZodRecord"] = "ZodRecord";
  ZodFirstPartyTypeKind2["ZodMap"] = "ZodMap";
  ZodFirstPartyTypeKind2["ZodSet"] = "ZodSet";
  ZodFirstPartyTypeKind2["ZodFunction"] = "ZodFunction";
  ZodFirstPartyTypeKind2["ZodLazy"] = "ZodLazy";
  ZodFirstPartyTypeKind2["ZodLiteral"] = "ZodLiteral";
  ZodFirstPartyTypeKind2["ZodEnum"] = "ZodEnum";
  ZodFirstPartyTypeKind2["ZodEffects"] = "ZodEffects";
  ZodFirstPartyTypeKind2["ZodNativeEnum"] = "ZodNativeEnum";
  ZodFirstPartyTypeKind2["ZodOptional"] = "ZodOptional";
  ZodFirstPartyTypeKind2["ZodNullable"] = "ZodNullable";
  ZodFirstPartyTypeKind2["ZodDefault"] = "ZodDefault";
  ZodFirstPartyTypeKind2["ZodCatch"] = "ZodCatch";
  ZodFirstPartyTypeKind2["ZodPromise"] = "ZodPromise";
  ZodFirstPartyTypeKind2["ZodBranded"] = "ZodBranded";
  ZodFirstPartyTypeKind2["ZodPipeline"] = "ZodPipeline";
  ZodFirstPartyTypeKind2["ZodReadonly"] = "ZodReadonly";
})(ZodFirstPartyTypeKind || (ZodFirstPartyTypeKind = {}));
var instanceOfType = (cls, params = {
  message: `Input not instance of ${cls.name}`
}) => custom((data) => data instanceof cls, params);
var stringType = ZodString.create;
var numberType = ZodNumber.create;
var nanType = ZodNaN.create;
var bigIntType = ZodBigInt.create;
var booleanType = ZodBoolean.create;
var dateType = ZodDate.create;
var symbolType = ZodSymbol.create;
var undefinedType = ZodUndefined.create;
var nullType = ZodNull.create;
var anyType = ZodAny.create;
var unknownType = ZodUnknown.create;
var neverType = ZodNever.create;
var voidType = ZodVoid.create;
var arrayType = ZodArray.create;
var objectType = ZodObject.create;
var strictObjectType = ZodObject.strictCreate;
var unionType = ZodUnion.create;
var discriminatedUnionType = ZodDiscriminatedUnion.create;
var intersectionType = ZodIntersection.create;
var tupleType = ZodTuple.create;
var recordType = ZodRecord.create;
var mapType = ZodMap.create;
var setType = ZodSet.create;
var functionType = ZodFunction.create;
var lazyType = ZodLazy.create;
var literalType = ZodLiteral.create;
var enumType = ZodEnum.create;
var nativeEnumType = ZodNativeEnum.create;
var promiseType = ZodPromise.create;
var effectsType = ZodEffects.create;
var optionalType = ZodOptional.create;
var nullableType = ZodNullable.create;
var preprocessType = ZodEffects.createWithPreprocess;
var pipelineType = ZodPipeline.create;
var ostring = () => stringType().optional();
var onumber = () => numberType().optional();
var oboolean = () => booleanType().optional();
var coerce = {
  string: (arg) => ZodString.create({ ...arg, coerce: true }),
  number: (arg) => ZodNumber.create({ ...arg, coerce: true }),
  boolean: (arg) => ZodBoolean.create({
    ...arg,
    coerce: true
  }),
  bigint: (arg) => ZodBigInt.create({ ...arg, coerce: true }),
  date: (arg) => ZodDate.create({ ...arg, coerce: true })
};
var NEVER = INVALID;
// packages/grammar/src/paths.ts
import { existsSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
function findPackageRoot(fromUrl) {
  let dir = dirname(fileURLToPath(fromUrl));
  for (let i = 0;i < 8; i++) {
    const candidate = join(dir, "package.json");
    if (existsSync(candidate))
      return dir;
    dir = dirname(dir);
  }
  return dirname(fileURLToPath(fromUrl));
}

// packages/grammar/src/ginza/generated.ts
var GinzaPOSSchema = exports_external.enum(["ADJ", "ADP", "ADV", "AUX", "CCONJ", "DET", "INTJ", "NOUN", "NUM", "PART", "PRON", "PROPN", "PUNCT", "SCONJ", "SYM", "VERB", "X"]);
var GINZA_POS_LABELS = ["ADJ", "ADP", "ADV", "AUX", "CCONJ", "DET", "INTJ", "NOUN", "NUM", "PART", "PRON", "PROPN", "PUNCT", "SCONJ", "SYM", "VERB", "X"];
var GinzaDepSchema = exports_external.enum(["acl", "advcl", "advmod", "amod", "aux", "case", "cc", "ccomp", "compound", "cop", "csubj", "dep", "det", "discourse", "dislocated", "fixed", "mark", "nmod", "nsubj", "nummod", "obj", "obl", "punct", "root"]);
var GINZA_DEP_LABELS = ["acl", "advcl", "advmod", "amod", "aux", "case", "cc", "ccomp", "compound", "cop", "csubj", "dep", "det", "discourse", "dislocated", "fixed", "mark", "nmod", "nsubj", "nummod", "obj", "obl", "punct", "root"];
var GinzaNESchema = exports_external.enum(["B-DATE", "B-EVENT", "B-FAC", "B-GPE", "B-LANGUAGE", "B-LAW", "B-LOC", "B-MONEY", "B-NORP", "B-ORDINAL", "B-ORG", "B-OTHERS", "B-PERCENT", "B-PERSON", "B-PRODUCT", "B-QUANTITY", "B-TIME", "B-URL", "B-WORK_OF_ART", "I-DATE", "I-EVENT", "I-FAC", "I-GPE", "I-LANGUAGE", "I-LAW", "I-LOC", "I-MONEY", "I-NORP", "I-ORDINAL", "I-ORG", "I-OTHERS", "I-PERCENT", "I-PERSON", "I-PRODUCT", "I-QUANTITY", "I-TIME", "I-URL", "I-WORK_OF_ART", "O"]);
var GinzaBunsetuBISchema = exports_external.enum(["B", "I"]);
var GinzaBunsetuPositionTypeSchema = exports_external.enum(["SEM_HEAD", "SYN_HEAD", "ROOT", "CONT", "NO_HEAD"]);
var GinzaConjugationClassSchema = exports_external.enum(["カ行変格", "サ行変格", "上一段-ア行", "上一段-カ行", "上一段-ガ行", "上一段-ザ行", "上一段-タ行", "上一段-ナ行", "上一段-ハ行", "上一段-バ行", "上一段-マ行", "上一段-ラ行", "下一段-ア行", "下一段-カ行", "下一段-ガ行", "下一段-サ行", "下一段-タ行", "下一段-ダ行", "下一段-ナ行", "下一段-ハ行", "下一段-バ行", "下一段-マ行", "下一段-ラ行", "五段-カ行", "五段-ガ行", "五段-サ行", "五段-タ行", "五段-ナ行", "五段-バ行", "五段-マ行", "五段-ラ行", "五段-ワア行", "助動詞-ジャ", "助動詞-タ", "助動詞-タイ", "助動詞-ダ", "助動詞-デス", "助動詞-ナイ", "助動詞-ヌ", "助動詞-マイ", "助動詞-マス", "助動詞-ヤ", "助動詞-ラシイ", "助動詞-レル", "形容詞", "文語サ行変格", "文語ラ行変格", "文語上二段-ダ行", "文語下二段-ダ行", "文語下二段-ラ行", "文語助動詞-キ", "文語助動詞-ゴトシ", "文語助動詞-ジ", "文語助動詞-ズ", "文語助動詞-タリ-断定", "文語助動詞-ナリ-断定", "文語助動詞-ベシ", "文語助動詞-マジ", "文語助動詞-ム", "文語助動詞-リ", "文語四段-カ行", "文語四段-ハ行", "文語四段-ラ行", "文語形容詞-ク", "文語形容詞-シク"]);
var GINZA_CONJUGATION_CLASSES = ["カ行変格", "サ行変格", "上一段-ア行", "上一段-カ行", "上一段-ガ行", "上一段-ザ行", "上一段-タ行", "上一段-ナ行", "上一段-ハ行", "上一段-バ行", "上一段-マ行", "上一段-ラ行", "下一段-ア行", "下一段-カ行", "下一段-ガ行", "下一段-サ行", "下一段-タ行", "下一段-ダ行", "下一段-ナ行", "下一段-ハ行", "下一段-バ行", "下一段-マ行", "下一段-ラ行", "五段-カ行", "五段-ガ行", "五段-サ行", "五段-タ行", "五段-ナ行", "五段-バ行", "五段-マ行", "五段-ラ行", "五段-ワア行", "助動詞-ジャ", "助動詞-タ", "助動詞-タイ", "助動詞-ダ", "助動詞-デス", "助動詞-ナイ", "助動詞-ヌ", "助動詞-マイ", "助動詞-マス", "助動詞-ヤ", "助動詞-ラシイ", "助動詞-レル", "形容詞", "文語サ行変格", "文語ラ行変格", "文語上二段-ダ行", "文語下二段-ダ行", "文語下二段-ラ行", "文語助動詞-キ", "文語助動詞-ゴトシ", "文語助動詞-ジ", "文語助動詞-ズ", "文語助動詞-タリ-断定", "文語助動詞-ナリ-断定", "文語助動詞-ベシ", "文語助動詞-マジ", "文語助動詞-ム", "文語助動詞-リ", "文語四段-カ行", "文語四段-ハ行", "文語四段-ラ行", "文語形容詞-ク", "文語形容詞-シク"];
var GinzaInflectionFormSchema = exports_external.enum(["仮定形-一般", "仮定形-融合", "命令形", "已然形-一般", "意志推量形", "未然形-サ", "未然形-セ", "未然形-一般", "未然形-撥音便", "未然形-補助", "終止形-ウ音便", "終止形-一般", "終止形-撥音便", "語幹-サ", "語幹-一般", "連体形-一般", "連体形-撥音便", "連体形-省略", "連体形-補助", "連用形-イ音便", "連用形-ウ音便", "連用形-ニ", "連用形-一般", "連用形-促音便", "連用形-撥音便", "連用形-融合"]);
var GINZA_INFLECTION_FORMS = ["仮定形-一般", "仮定形-融合", "命令形", "已然形-一般", "意志推量形", "未然形-サ", "未然形-セ", "未然形-一般", "未然形-撥音便", "未然形-補助", "終止形-ウ音便", "終止形-一般", "終止形-撥音便", "語幹-サ", "語幹-一般", "連体形-一般", "連体形-撥音便", "連体形-省略", "連体形-補助", "連用形-イ音便", "連用形-ウ音便", "連用形-ニ", "連用形-一般", "連用形-促音便", "連用形-撥音便", "連用形-融合"];
var CONJ_CLASS_SET = new Set(GINZA_CONJUGATION_CLASSES);
var INFL_FORM_SET = new Set(GINZA_INFLECTION_FORMS);
function parseInflection(inf) {
  if (!inf)
    return null;
  const parts = inf.split(/[;,]/);
  const cc = parts[0]?.trim() ?? "";
  const form = parts[1]?.trim() ?? "";
  return {
    conjugationClass: CONJ_CLASS_SET.has(cc) ? cc : null,
    inflectionForm: INFL_FORM_SET.has(form) ? form : null,
    raw: inf
  };
}

// packages/grammar/src/ginza/client.ts
var WorkerResponseSchema = exports_external.object({
  id: exports_external.string().optional().nullable(),
  ok: exports_external.boolean(),
  docs: exports_external.array(exports_external.any()).optional(),
  meta: exports_external.any().optional(),
  error: exports_external.string().optional()
});
var WorkerMetaSchema = exports_external.object({
  model: exports_external.string().nullable().optional(),
  lang: exports_external.string().nullable().optional(),
  spacyVersion: exports_external.string().nullable().optional(),
  ginzaVersion: exports_external.string().nullable().optional(),
  jaGinzaModelVersion: exports_external.string().nullable().optional(),
  pipes: exports_external.array(exports_external.string()),
  labels: exports_external.record(exports_external.array(exports_external.string()))
});
var WorkerTokenSchema = exports_external.object({
  i: exports_external.number().int().nonnegative(),
  text: exports_external.string(),
  lemma: exports_external.string(),
  pos: exports_external.string(),
  tag: exports_external.string(),
  dep: exports_external.string(),
  head: exports_external.number().int(),
  start: exports_external.number().int(),
  end: exports_external.number().int(),
  norm: exports_external.string().optional(),
  whitespace: exports_external.string().optional(),
  feats: exports_external.record(exports_external.string()).optional(),
  inflection: exports_external.string().optional(),
  reading: exports_external.string().optional(),
  ne: exports_external.string().optional(),
  ene: exports_external.string().optional(),
  bunsetu: exports_external.object({
    bi: exports_external.string().nullable().optional(),
    positionType: exports_external.string().nullable().optional()
  }).optional(),
  clauseHead: exports_external.number().int().optional(),
  misc: exports_external.record(exports_external.union([exports_external.string(), exports_external.literal(true)])).optional()
});
var WorkerSentenceSchema = exports_external.object({
  text: exports_external.string(),
  start: exports_external.number().int(),
  end: exports_external.number().int(),
  tokens: exports_external.array(WorkerTokenSchema)
});
var WorkerDocSchema = exports_external.object({
  text: exports_external.string(),
  sentences: exports_external.array(WorkerSentenceSchema)
});
var POS_SET = new Set(GINZA_POS_LABELS);
var DEP_SET = new Set(GINZA_DEP_LABELS);
var CONJ_SET = new Set(GINZA_CONJUGATION_CLASSES);
var INFL_FORM_SET2 = new Set(GINZA_INFLECTION_FORMS);
var WARNED = {
  pos: new Set,
  dep: new Set,
  conjugationClass: new Set,
  inflectionForm: new Set,
  inflectionExtra: new Set
};
function warnOnce(kind, value, msg) {
  const set = WARNED[kind];
  if (set.has(value))
    return;
  set.add(value);
  console.warn(msg);
}
function enrichToken(t) {
  if (t.pos && !POS_SET.has(t.pos)) {
    warnOnce("pos", t.pos, `[grammar][ginza] unseen POS label: '${t.pos}'`);
  }
  if (t.dep && !DEP_SET.has(t.dep)) {
    warnOnce("dep", t.dep, `[grammar][ginza] unseen dependency label: '${t.dep}'`);
  }
  if (t.inflection) {
    const parts = t.inflection.split(/[;,]/).map((p) => p.trim()).filter(Boolean);
    const cc = parts[0];
    const form = parts[1];
    if (cc && !CONJ_SET.has(cc)) {
      warnOnce("conjugationClass", cc, `[grammar][ginza] unseen conjugation class: '${cc}' (raw inflection='${t.inflection}')`);
    }
    if (form && !INFL_FORM_SET2.has(form)) {
      warnOnce("inflectionForm", form, `[grammar][ginza] unseen inflection form: '${form}' (raw inflection='${t.inflection}')`);
    }
    if (parts.length > 2) {
      warnOnce("inflectionExtra", t.inflection, `[grammar][ginza] inflection has extra parts (not modeled): '${t.inflection}'`);
    }
  }
  const parsed = parseInflection(t.inflection);
  return {
    ...t,
    pos: t.pos,
    dep: t.dep,
    conjugationClass: parsed?.conjugationClass ?? undefined,
    inflectionForm: parsed?.inflectionForm ?? undefined
  };
}
function enrichSentence(s) {
  return {
    ...s,
    tokens: s.tokens.map(enrichToken)
  };
}
function enrichDoc(d) {
  return {
    ...d,
    sentences: d.sentences.map(enrichSentence)
  };
}

class GinzaClient {
  proc = null;
  rl = null;
  pending = new Map;
  nextId = 1;
  python;
  workerPath;
  constructor(opts = {}) {
    this.python = opts.python ?? "python3";
    const pkgRoot = findPackageRoot(import.meta.url);
    this.workerPath = opts.workerPath ?? join2(pkgRoot, "python", "ginza_worker.py");
  }
  async start() {
    if (this.proc)
      return;
    this.proc = spawn(this.python, ["-u", this.workerPath], {
      stdio: ["pipe", "pipe", "pipe"],
      env: process.env
    });
    this.proc.on("exit", (code, signal) => {
      const err = new Error(`ginza worker exited (code=${code}, signal=${signal})`);
      for (const { reject } of this.pending.values())
        reject(err);
      this.pending.clear();
      this.proc = null;
      this.rl?.close();
      this.rl = null;
    });
    this.rl = createInterface({ input: this.proc.stdout });
    this.rl.on("line", (line) => {
      let parsed;
      try {
        parsed = JSON.parse(line);
      } catch {
        return;
      }
      const msg = WorkerResponseSchema.safeParse(parsed);
      if (!msg.success)
        return;
      const { id, ok, docs, meta, error } = msg.data;
      if (!id)
        return;
      const entry = this.pending.get(id);
      if (!entry)
        return;
      this.pending.delete(id);
      if (!ok) {
        entry.reject(new Error(error ?? "ginza worker error"));
        return;
      }
      if (entry.kind === "meta") {
        entry.resolve(WorkerMetaSchema.parse(meta));
        return;
      }
      entry.resolve((docs ?? []).map((d) => enrichDoc(WorkerDocSchema.parse(d))));
    });
    this.proc.once("error", (e) => {
      throw e;
    });
    await Promise.race([once(this.proc.stderr, "data"), new Promise((r) => setTimeout(r, 50))]);
  }
  async stop() {
    if (!this.proc)
      return;
    this.proc.kill("SIGTERM");
    this.proc = null;
    this.rl?.close();
    this.rl = null;
    this.pending.clear();
  }
  async analyze(texts) {
    if (!this.proc || !this.rl)
      await this.start();
    if (!this.proc)
      throw new Error("ginza worker not running");
    const id = `req-${this.nextId++}`;
    const payload = JSON.stringify({ id, op: "analyze", texts });
    const p = new Promise((resolve, reject) => {
      this.pending.set(id, { kind: "analyze", resolve, reject });
    });
    this.proc.stdin.write(payload + `
`);
    return await p;
  }
  async meta() {
    if (!this.proc || !this.rl)
      await this.start();
    if (!this.proc)
      throw new Error("ginza worker not running");
    const id = `req-${this.nextId++}`;
    const payload = JSON.stringify({ id, op: "meta" });
    const p = new Promise((resolve, reject) => {
      this.pending.set(id, { kind: "meta", resolve, reject });
    });
    this.proc.stdin.write(payload + `
`);
    return await p;
  }
}

// packages/grammar/src/engine/dsl.ts
function V(v) {
  return { v };
}
function text(value) {
  return { kind: "text", value };
}
function textOneOf(value) {
  return { kind: "textOneOf", value };
}
function lemma(value) {
  return { kind: "lemma", value };
}
function lemmaOneOf(value) {
  return { kind: "lemmaOneOf", value };
}
function pos(value) {
  return { kind: "pos", value };
}
function dep(value) {
  return { kind: "dep", value };
}
function depOneOf(value) {
  return { kind: "depOneOf", value };
}
function posOneOf(value) {
  return { kind: "posOneOf", value };
}
function inflectionForm(value) {
  return { kind: "inflectionForm", value };
}
function conjugationClass(value) {
  return { kind: "conjugationClass", value };
}
function conjugationClassOneOf(value) {
  return { kind: "conjugationClassOneOf", value };
}
function tag(value) {
  return { kind: "tag", value };
}
function node(node2, preds) {
  return { kind: "node", node: node2, preds };
}
function edge(child, head, depLabel) {
  return { kind: "edge", child, head, dep: depLabel };
}
function before(a, b, maxDistance) {
  return { kind: "before", a, b, maxDistance };
}
function not(clause) {
  return { kind: "not", clause };
}
function tokenMatchesPreds(tok, preds) {
  for (const p of preds) {
    if (p.kind === "text" && tok.text !== p.value)
      return false;
    if (p.kind === "textRe" && !p.value.test(tok.text))
      return false;
    if (p.kind === "textOneOf" && !p.value.includes(tok.text))
      return false;
    if (p.kind === "lemma" && tok.lemma !== p.value)
      return false;
    if (p.kind === "lemmaRe" && !p.value.test(tok.lemma))
      return false;
    if (p.kind === "lemmaOneOf" && !p.value.includes(tok.lemma))
      return false;
    if (p.kind === "pos" && tok.pos !== p.value)
      return false;
    if (p.kind === "posOneOf" && !p.value.includes(tok.pos))
      return false;
    if (p.kind === "dep" && tok.dep !== p.value)
      return false;
    if (p.kind === "depOneOf" && !p.value.includes(tok.dep))
      return false;
    if (p.kind === "inflectionForm" && tok.inflectionForm !== p.value)
      return false;
    if (p.kind === "conjugationClass" && tok.conjugationClass !== p.value)
      return false;
    if (p.kind === "conjugationClassOneOf" && !p.value.includes(tok.conjugationClass))
      return false;
    if (p.kind === "tag" && tok.tag !== p.value)
      return false;
  }
  return true;
}

// packages/grammar/src/engine/compiler.ts
function extractTriggersFromClauses(clauses, out) {
  for (const c of clauses) {
    if (c.kind === "node") {
      for (const p of c.preds) {
        if (p.kind === "lemma")
          out.push({ kind: "lemma", value: p.value });
        if (p.kind === "text")
          out.push({ kind: "text", value: p.value });
        if (p.kind === "lemmaOneOf") {
          for (const v of p.value)
            out.push({ kind: "lemma", value: v });
        }
        if (p.kind === "textOneOf") {
          for (const v of p.value)
            out.push({ kind: "text", value: v });
        }
      }
    } else if (c.kind === "either") {
      for (const branch of c.branches) {
        extractTriggersFromClauses(branch.clauses, out);
      }
    }
  }
}
function deriveTriggers(spec) {
  const out = [];
  extractTriggersFromClauses(spec.where, out);
  const seen = new Set;
  const deduped = [];
  for (const t of out) {
    const k = `${t.kind}:${t.value}`;
    if (seen.has(k))
      continue;
    seen.add(k);
    deduped.push(t);
  }
  deduped.sort((a, b) => a.kind === b.kind ? 0 : a.kind === "lemma" ? -1 : 1);
  return deduped;
}
function pushMapArr(m, k, v) {
  const arr = m.get(k);
  if (arr)
    arr.push(v);
  else
    m.set(k, [v]);
}
function buildSentenceIndex(sent) {
  const byLemma = new Map;
  const byText = new Map;
  const byPos = new Map;
  const byDep = new Map;
  const childrenOf = new Map;
  const childrenOfByDep = new Map;
  const all = [];
  for (let i = 0;i < sent.tokens.length; i++) {
    const t = sent.tokens[i];
    all.push(i);
    pushMapArr(byLemma, t.lemma, i);
    pushMapArr(byText, t.text, i);
    pushMapArr(byPos, t.pos, i);
    pushMapArr(byDep, t.dep, i);
    if (t.head >= 0) {
      pushMapArr(childrenOf, t.head, i);
      let depMap = childrenOfByDep.get(t.head);
      if (!depMap) {
        depMap = new Map;
        childrenOfByDep.set(t.head, depMap);
      }
      pushMapArr(depMap, t.dep, i);
    }
  }
  return { all, byLemma, byText, byPos, byDep, childrenOf, childrenOfByDep };
}
function intersect(a, b) {
  if (a.length === 0 || b.length === 0)
    return [];
  const [small, big] = a.length <= b.length ? [a, b] : [b, a];
  const s = new Set(small);
  const out = [];
  for (const x of big)
    if (s.has(x))
      out.push(x);
  return out;
}
function windowCandidates(n, startExclusive, endInclusive) {
  const out = [];
  const lo = Math.max(0, startExclusive + 1);
  const hi = Math.min(n - 1, endInclusive);
  for (let i = lo;i <= hi; i++)
    out.push(i);
  return out;
}
function getClauseVars(c) {
  if (c.kind === "node")
    return [c.node.v];
  if (c.kind === "edge")
    return [c.child.v, c.head.v];
  if (c.kind === "next" || c.kind === "before")
    return [c.a.v, c.b.v];
  if (c.kind === "not")
    return getClauseVars(c.clause);
  if (c.kind === "optional")
    return c.clauses.flatMap(getClauseVars);
  if (c.kind === "either")
    return c.branches.flatMap((b) => b.clauses.flatMap(getClauseVars));
  return [];
}
function clauseHolds(clause, sent, bind) {
  if (clause.kind === "node") {
    const idx = bind.get(clause.node.v);
    if (idx === undefined)
      return true;
    const tok = sent.tokens[idx];
    if (!tok)
      return false;
    return tokenMatchesPreds(tok, clause.preds);
  }
  if (clause.kind === "edge") {
    const cIdx = bind.get(clause.child.v);
    const hIdx = bind.get(clause.head.v);
    if (cIdx === undefined || hIdx === undefined)
      return true;
    const tok = sent.tokens[cIdx];
    if (!tok)
      return false;
    if (tok.head !== hIdx)
      return false;
    if (clause.dep && tok.dep !== clause.dep)
      return false;
    return true;
  }
  if (clause.kind === "next") {
    const a = bind.get(clause.a.v);
    const b = bind.get(clause.b.v);
    if (a === undefined || b === undefined)
      return true;
    return b === a + 1;
  }
  if (clause.kind === "before") {
    const a = bind.get(clause.a.v);
    const b = bind.get(clause.b.v);
    if (a === undefined || b === undefined)
      return true;
    if (a >= b)
      return false;
    if (clause.maxDistance !== undefined && b - a > clause.maxDistance)
      return false;
    return true;
  }
  if (clause.kind === "not") {
    const innerVars = getClauseVars(clause.clause);
    if (innerVars.some((v) => !bind.has(v)))
      return true;
    return !clauseHolds(clause.clause, sent, bind);
  }
  if (clause.kind === "optional") {
    return true;
  }
  if (clause.kind === "either") {
    return clause.branches.some((branch) => allClausesHold(branch.clauses, sent, bind));
  }
  const _exhaustive = clause;
  throw new Error(`[grammar] Unknown clause kind: ${_exhaustive.kind}`);
}
function allClausesHold(clauses, sent, bind) {
  for (const c of clauses) {
    if (!clauseHolds(c, sent, bind))
      return false;
  }
  return true;
}
function extractVarsFromClauses(clauses, vs) {
  for (const c of clauses) {
    if (c.kind === "node")
      vs.add(c.node.v);
    else if (c.kind === "edge") {
      vs.add(c.child.v);
      vs.add(c.head.v);
    } else if (c.kind === "next" || c.kind === "before") {
      vs.add(c.a.v);
      vs.add(c.b.v);
    } else if (c.kind === "either") {
      for (const branch of c.branches) {
        extractVarsFromClauses(branch.clauses, vs);
      }
    }
  }
}
function varsInSpec(spec) {
  const vs = new Set;
  extractVarsFromClauses(spec.where, vs);
  return [...vs];
}
function findAnchorInClauses(clauses) {
  for (const c of clauses) {
    if (c.kind === "node") {
      if (c.preds.some((p) => p.kind === "lemma" || p.kind === "text"))
        return c.node.v;
    } else if (c.kind === "either") {
      for (const branch of c.branches) {
        const anchor = findAnchorInClauses(branch.clauses);
        if (anchor)
          return anchor;
      }
    }
  }
  return null;
}
function anchorVar(spec) {
  return findAnchorInClauses(spec.where);
}
function buildCompiledSpec(spec) {
  const vars = varsInSpec(spec);
  const anchor = anchorVar(spec);
  const orderedVars = anchor ? [anchor, ...vars.filter((v) => v !== anchor).sort()] : [...vars].sort();
  const varToPredsFlat = new Map;
  const varToEdgesAsChild = new Map;
  const varToEdgesAsHead = new Map;
  const varToBeforeAsA = new Map;
  const varToBeforeAsB = new Map;
  const varToNextAsA = new Map;
  const varToNextAsB = new Map;
  function indexClauses(clauses) {
    for (const c of clauses) {
      if (c.kind === "node") {
        const preds = varToPredsFlat.get(c.node.v) ?? [];
        preds.push(...c.preds);
        varToPredsFlat.set(c.node.v, preds);
      } else if (c.kind === "edge") {
        const childArr = varToEdgesAsChild.get(c.child.v) ?? [];
        childArr.push(c);
        varToEdgesAsChild.set(c.child.v, childArr);
        const headArr = varToEdgesAsHead.get(c.head.v) ?? [];
        headArr.push(c);
        varToEdgesAsHead.set(c.head.v, headArr);
      } else if (c.kind === "before") {
        const aArr = varToBeforeAsA.get(c.a.v) ?? [];
        aArr.push(c);
        varToBeforeAsA.set(c.a.v, aArr);
        const bArr = varToBeforeAsB.get(c.b.v) ?? [];
        bArr.push(c);
        varToBeforeAsB.set(c.b.v, bArr);
      } else if (c.kind === "next") {
        const aArr = varToNextAsA.get(c.a.v) ?? [];
        aArr.push(c);
        varToNextAsA.set(c.a.v, aArr);
        const bArr = varToNextAsB.get(c.b.v) ?? [];
        bArr.push(c);
        varToNextAsB.set(c.b.v, bArr);
      } else if (c.kind === "optional") {
        indexClauses(c.clauses);
      } else if (c.kind === "either") {
        for (const branch of c.branches)
          indexClauses(branch.clauses);
      }
    }
  }
  indexClauses(spec.where);
  const sortedCaptureNames = spec.captures.map((c) => c.name).sort();
  return {
    spec,
    orderedVars,
    sortedCaptureNames,
    varToPredsFlat,
    varToEdgesAsChild,
    varToEdgesAsHead,
    varToBeforeAsA,
    varToBeforeAsB,
    varToNextAsA,
    varToNextAsB
  };
}
function possibleBindingsForVar(sent, idx, varName, compiled, bind) {
  const preds = compiled.varToPredsFlat.get(varName) ?? [];
  let candidates = null;
  for (const p of preds) {
    if (p.kind === "lemma")
      candidates = candidates ? intersect(candidates, idx.byLemma.get(p.value) ?? []) : idx.byLemma.get(p.value) ?? [];
    else if (p.kind === "text")
      candidates = candidates ? intersect(candidates, idx.byText.get(p.value) ?? []) : idx.byText.get(p.value) ?? [];
    else if (p.kind === "pos")
      candidates = candidates ? intersect(candidates, idx.byPos.get(p.value) ?? []) : idx.byPos.get(p.value) ?? [];
    else if (p.kind === "dep")
      candidates = candidates ? intersect(candidates, idx.byDep.get(p.value) ?? []) : idx.byDep.get(p.value) ?? [];
  }
  if (!candidates)
    candidates = idx.all;
  for (const c of compiled.varToEdgesAsChild.get(varName) ?? []) {
    const hIdx = bind.get(c.head.v);
    if (hIdx !== undefined) {
      const fromHead = c.dep !== undefined ? idx.childrenOfByDep.get(hIdx)?.get(c.dep) ?? [] : idx.childrenOf.get(hIdx) ?? [];
      candidates = intersect(candidates, fromHead);
    }
  }
  for (const c of compiled.varToEdgesAsHead.get(varName) ?? []) {
    const cIdx = bind.get(c.child.v);
    if (cIdx !== undefined) {
      const tok = sent.tokens[cIdx];
      if (!tok)
        return [];
      if (c.dep !== undefined && tok.dep !== c.dep)
        return [];
      const h = tok.head;
      candidates = intersect(candidates, h >= 0 ? [h] : []);
    }
  }
  for (const c of compiled.varToBeforeAsB.get(varName) ?? []) {
    const aIdx = bind.get(c.a.v);
    if (aIdx !== undefined) {
      const max = c.maxDistance !== undefined ? aIdx + c.maxDistance : sent.tokens.length - 1;
      candidates = intersect(candidates, windowCandidates(sent.tokens.length, aIdx, max));
    }
  }
  for (const c of compiled.varToBeforeAsA.get(varName) ?? []) {
    const bIdx = bind.get(c.b.v);
    if (bIdx !== undefined) {
      const min = c.maxDistance !== undefined ? bIdx - c.maxDistance : 0;
      candidates = intersect(candidates, windowCandidates(sent.tokens.length, min - 1, bIdx - 1));
    }
  }
  for (const c of compiled.varToNextAsB.get(varName) ?? []) {
    const aIdx = bind.get(c.a.v);
    if (aIdx !== undefined)
      candidates = intersect(candidates, aIdx + 1 < sent.tokens.length ? [aIdx + 1] : []);
  }
  for (const c of compiled.varToNextAsA.get(varName) ?? []) {
    const bIdx = bind.get(c.b.v);
    if (bIdx !== undefined)
      candidates = intersect(candidates, bIdx - 1 >= 0 ? [bIdx - 1] : []);
  }
  const out = [];
  for (const i of candidates) {
    const tok = sent.tokens[i];
    if (!tok)
      continue;
    if (preds.length === 0 || tokenMatchesPreds(tok, preds))
      out.push(i);
  }
  return out;
}
function applyCaptureSpecs(captureSpecs, sent, bind, sourceText, captures) {
  for (const cap of captureSpecs) {
    if (cap.kind === "token") {
      const tokIdx = bind.get(cap.var.v);
      if (tokIdx === undefined) {
        throw new Error(`[grammar] Capture '${cap.name}' references unbound var '${cap.var.v}'`);
      }
      const tok = sent.tokens[tokIdx];
      if (!tok)
        return false;
      captures[cap.name] = {
        start: tok.start,
        end: tok.end,
        text: sourceText.slice(tok.start, tok.end)
      };
    } else if (cap.kind === "span") {
      const fromIdx = bind.get(cap.from.v);
      const toIdx = bind.get(cap.to.v);
      if (fromIdx === undefined) {
        throw new Error(`[grammar] Capture '${cap.name}' references unbound var '${cap.from.v}'`);
      }
      if (toIdx === undefined) {
        throw new Error(`[grammar] Capture '${cap.name}' references unbound var '${cap.to.v}'`);
      }
      const fromTok = sent.tokens[fromIdx];
      const toTok = sent.tokens[toIdx];
      if (!fromTok || !toTok)
        return false;
      const start = Math.min(fromTok.start, toTok.start);
      const end = Math.max(fromTok.end, toTok.end);
      captures[cap.name] = {
        start,
        end,
        text: sourceText.slice(start, end)
      };
    }
  }
  return true;
}
function buildCaptures(spec, sent, bind, sourceText) {
  const captures = {};
  if (!applyCaptureSpecs(spec.captures, sent, bind, sourceText, captures)) {
    return null;
  }
  return captures;
}
function findMatches(compiled, sent, sourceText, idx) {
  const { spec, orderedVars, sortedCaptureNames } = compiled;
  const matches = [];
  const bind = new Map;
  function dfs(k) {
    if (k === orderedVars.length) {
      const captures = buildCaptures(spec, sent, bind, sourceText);
      if (captures)
        matches.push(captures);
      return;
    }
    const v = orderedVars[k];
    for (const cand of possibleBindingsForVar(sent, idx, v, compiled, bind)) {
      bind.set(v, cand);
      if (allClausesHold(spec.where, sent, bind))
        dfs(k + 1);
      bind.delete(v);
    }
  }
  dfs(0);
  const seen = new Set;
  return matches.filter((m) => {
    const key = sortedCaptureNames.map((k) => `${k}:${m[k].start}-${m[k].end}`).join("|");
    if (seen.has(key))
      return false;
    seen.add(key);
    return true;
  });
}
function compileSingleRule(spec) {
  const triggers = deriveTriggers(spec);
  if (triggers.length === 0) {
    throw new Error(`[grammar] Rule '${spec.id}' has no literal lemma/text triggers. ` + `Rules must include at least one exact { lemma: "..." } or { text: "..." } predicate ` + `so the trigger-indexed dispatch can consider them.`);
  }
  const compiled = buildCompiledSpec(spec);
  return {
    id: spec.id,
    triggers,
    match: (sent, sourceText, idx) => {
      const index = idx ?? buildSentenceIndex(sent);
      return findMatches(compiled, sent, sourceText, index);
    }
  };
}
function compileRule(spec) {
  const eitherClause = spec.where.find((c) => c.kind === "either");
  if (!eitherClause) {
    return [compileSingleRule(spec)];
  }
  const otherClauses = spec.where.filter((c) => c.kind !== "either");
  return eitherClause.branches.map((branch) => {
    const branchSpec = {
      id: spec.id,
      where: [...otherClauses, ...branch.clauses],
      captures: [...spec.captures, ...branch.captures]
    };
    return compileSingleRule(branchSpec);
  });
}
function describeClause(c) {
  switch (c.kind) {
    case "node": {
      const preds = c.preds.map((p) => {
        if (p.kind === "text")
          return `text="${p.value}"`;
        if (p.kind === "textOneOf")
          return `text∈[${p.value.join(",")}]`;
        if (p.kind === "lemma")
          return `lemma="${p.value}"`;
        if (p.kind === "lemmaOneOf")
          return `lemma∈[${p.value.join(",")}]`;
        if (p.kind === "pos")
          return `pos=${p.value}`;
        if (p.kind === "dep")
          return `dep=${p.value}`;
        if (p.kind === "inflectionForm")
          return `inflForm=${p.value}`;
        if (p.kind === "conjugationClass")
          return `conjClass=${p.value}`;
        return `${p.kind}=...`;
      });
      return `node(${c.node.v}: ${preds.join(", ")})`;
    }
    case "edge":
      return `edge(${c.child.v} --${c.dep ?? "*"}--> ${c.head.v})`;
    case "before": {
      const dist = c.maxDistance !== undefined ? `≤${c.maxDistance}` : "";
      return `before(${c.a.v} < ${c.b.v}${dist})`;
    }
    case "next":
      return `next(${c.a.v}, ${c.b.v})`;
    case "not":
      return `not(${describeClause(c.clause)})`;
    case "optional":
      return `optional(${c.clauses.length} clauses)`;
    case "either":
      return `either(${c.branches.length} branches)`;
    default: {
      const _exhaustive = c;
      throw new Error(`Unknown clause kind: ${_exhaustive.kind}`);
    }
  }
}
function explainMatch(spec, sent, sourceText) {
  const eitherClause = spec.where.find((c) => c.kind === "either");
  if (eitherClause) {
    const otherClauses = spec.where.filter((c) => c.kind !== "either");
    let bestFailure = null;
    for (const branch of eitherClause.branches) {
      const branchSpec = {
        id: spec.id,
        where: [...otherClauses, ...branch.clauses],
        captures: [...spec.captures, ...branch.captures]
      };
      const result = explainMatch(branchSpec, sent, sourceText);
      if (result.matched)
        return result;
      if (!bestFailure || Object.keys(result.partialBinding).length > Object.keys(bestFailure.partialBinding).length) {
        bestFailure = result;
      }
    }
    return bestFailure ?? { matched: false, reason: "No branches matched", partialBinding: {}, triedCandidates: {} };
  }
  const idx = buildSentenceIndex(sent);
  const compiled = buildCompiledSpec(spec);
  const { orderedVars } = compiled;
  const bind = new Map;
  const triedCandidates = {};
  let deepestFailure = null;
  function recordFailure(reason, clause) {
    const failure = {
      matched: false,
      reason,
      failedClause: clause,
      partialBinding: Object.fromEntries([...bind.entries()].map(([v, i]) => [v, { tokenIdx: i, text: sent.tokens[i]?.text ?? "" }])),
      triedCandidates: { ...triedCandidates }
    };
    const depth = Object.keys(failure.partialBinding).length;
    const deepestDepth = deepestFailure ? Object.keys(deepestFailure.partialBinding).length : -1;
    if (depth >= deepestDepth) {
      deepestFailure = failure;
    }
  }
  function dfs(k) {
    if (k === orderedVars.length) {
      for (const c of spec.where) {
        if (!clauseHolds(c, sent, bind)) {
          recordFailure(`Clause failed: ${describeClause(c)}`, c);
          return null;
        }
      }
      return buildCaptures(spec, sent, bind, sourceText);
    }
    const v = orderedVars[k];
    const candidates = possibleBindingsForVar(sent, idx, v, compiled, bind);
    triedCandidates[v] = candidates;
    if (candidates.length === 0) {
      recordFailure(`No candidates for variable '${v}'`);
      return null;
    }
    for (const cand of candidates) {
      bind.set(v, cand);
      if (allClausesHold(spec.where, sent, bind)) {
        const result = dfs(k + 1);
        if (result)
          return result;
      } else {
        for (const c of spec.where) {
          if (!clauseHolds(c, sent, bind)) {
            recordFailure(`Clause failed: ${describeClause(c)}`, c);
            break;
          }
        }
      }
      bind.delete(v);
    }
    return null;
  }
  const captures = dfs(0);
  if (captures) {
    return { matched: true, captures };
  }
  return deepestFailure ?? {
    matched: false,
    reason: "No matches found",
    partialBinding: {},
    triedCandidates
  };
}

// packages/grammar/src/engine/lang.ts
function condToPreds(cond) {
  const out = [];
  if (cond.text !== undefined)
    out.push(text(cond.text));
  if (cond.textOneOf !== undefined)
    out.push(textOneOf(cond.textOneOf));
  if (cond.lemma !== undefined)
    out.push(lemma(cond.lemma));
  if (cond.lemmaOneOf !== undefined)
    out.push(lemmaOneOf(cond.lemmaOneOf));
  if (cond.pos !== undefined)
    out.push(pos(cond.pos));
  if (cond.posOneOf !== undefined)
    out.push(posOneOf(cond.posOneOf));
  if (cond.dep !== undefined)
    out.push(dep(cond.dep));
  if (cond.depOneOf !== undefined)
    out.push(depOneOf(cond.depOneOf));
  if (cond.inflectionForm !== undefined)
    out.push(inflectionForm(cond.inflectionForm));
  if (cond.conjugationClass !== undefined)
    out.push(conjugationClass(cond.conjugationClass));
  if (cond.conjugationClassOneOf !== undefined)
    out.push(conjugationClassOneOf(cond.conjugationClassOneOf));
  if (cond.tag !== undefined)
    out.push(tag(cond.tag));
  return out;
}

class LinguisticRuleBuilder {
  id;
  vars = [];
  clauses = [];
  captureSpecs = [];
  varSeq = 0;
  constructor(id) {
    this.id = id;
  }
  tok(cond, name) {
    const n = name ?? `v${this.varSeq++}`;
    const ref = V(n);
    const v = { name: n, ref, cond };
    this.vars.push(v);
    this.clauses.push(node(ref, condToPreds(cond)));
    return v;
  }
  verb(cond = {}, name) {
    return this.tok({ ...cond, pos: "VERB" }, name);
  }
  noun(cond = {}, name) {
    return this.tok({ ...cond, pos: "NOUN" }, name);
  }
  aux(cond = {}, name) {
    return this.tok({ ...cond, pos: "AUX" }, name);
  }
  adj(cond = {}, name) {
    return this.tok({ ...cond, pos: "ADJ" }, name);
  }
  adv(cond = {}, name) {
    return this.tok({ ...cond, pos: "ADV" }, name);
  }
  particle(particleText, name, cond) {
    return this.tok({ text: particleText, ...cond }, name);
  }
  headChild(head, child, depLabel) {
    this.clauses.push(edge(child.ref, head.ref, depLabel));
    return this;
  }
  caseMarker(nominal, particle) {
    return this.headChild(nominal, particle, "case");
  }
  auxOf(head, auxTok) {
    return this.headChild(head, auxTok, "aux");
  }
  copulaOf(head, copTok) {
    return this.headChild(head, copTok, "cop");
  }
  objectOf(verb, obj) {
    return this.headChild(verb, obj, "obj");
  }
  inOrder(a, b, maxDistance) {
    this.clauses.push(before(a.ref, b.ref, maxDistance));
    return this;
  }
  not(build) {
    const prevLen = this.clauses.length;
    build(this);
    const clausesToNegate = this.clauses.splice(prevLen);
    if (clausesToNegate.length > 0) {
      if (clausesToNegate.length === 1) {
        this.clauses.push(not(clausesToNegate[0]));
      } else {
        for (const c of clausesToNegate) {
          this.clauses.push(not(c));
        }
      }
    }
    return this;
  }
  optional(build) {
    const prevLen = this.clauses.length;
    build(this);
    const optionalClauses = this.clauses.splice(prevLen);
    if (optionalClauses.length > 0) {
      this.clauses.push({ kind: "optional", clauses: optionalClauses });
    }
    return this;
  }
  either(...branches) {
    const eitherBranches = [];
    for (const buildBranch of branches) {
      const branchBuilder = new LinguisticRuleBuilder(this.id);
      branchBuilder.varSeq = this.varSeq;
      buildBranch(branchBuilder);
      this.varSeq = branchBuilder.varSeq;
      eitherBranches.push({
        clauses: branchBuilder.clauses,
        captures: branchBuilder.captureSpecs
      });
    }
    this.clauses.push({ kind: "either", branches: eitherBranches });
    return this;
  }
  capture(v) {
    this.captureSpecs.push({ kind: "token", name: "match", var: v.ref });
    return this;
  }
  captureAs(name, v) {
    this.captureSpecs.push({ kind: "token", name, var: v.ref });
    return this;
  }
  captureSpan(name, from, to) {
    this.captureSpecs.push({ kind: "span", name, from: from.ref, to: to.ref });
    return this;
  }
  build() {
    return {
      id: this.id,
      where: this.clauses,
      captures: this.captureSpecs
    };
  }
}
function linguisticRule(id, build) {
  const r = new LinguisticRuleBuilder(id);
  build(r);
  return r.build();
}
// packages/grammar/src/ruleset.ts
function triggerKey(t) {
  return `${t.kind}:${t.value}`;
}
function compileRuleset(rs) {
  return {
    id: rs.id,
    rules: rs.rules.flatMap(compileRule)
  };
}
function buildProgram(rulesets) {
  const compiled = rulesets.map(compileRuleset);
  const dispatch = new Map;
  for (let rsIdx = 0;rsIdx < compiled.length; rsIdx++) {
    const rs = compiled[rsIdx];
    for (let rIdx = 0;rIdx < rs.rules.length; rIdx++) {
      const rule = rs.rules[rIdx];
      for (const t of rule.triggers) {
        const k = triggerKey(t);
        const arr = dispatch.get(k) ?? [];
        arr.push([rsIdx, rIdx]);
        dispatch.set(k, arr);
      }
    }
  }
  return { rulesets: compiled, dispatch };
}
function sentenceTokenTriggerKeys(sent) {
  const keys = [];
  for (const tok of sent.tokens) {
    keys.push(`lemma:${tok.lemma}`);
    keys.push(`text:${tok.text}`);
  }
  return keys;
}
function matchSentence(program, sent, sourceText, opts = {}) {
  const enabledRulesets = opts.rulesetIds ? new Set(opts.rulesetIds) : null;
  const candidates = new Set;
  for (const k of sentenceTokenTriggerKeys(sent)) {
    const pairs = program.dispatch.get(k);
    if (!pairs)
      continue;
    for (const [rsIdx, rIdx] of pairs) {
      if (enabledRulesets && !enabledRulesets.has(program.rulesets[rsIdx].id))
        continue;
      candidates.add(`${rsIdx}:${rIdx}`);
    }
  }
  const idx = buildSentenceIndex(sent);
  const hits = [];
  for (const key of candidates) {
    const [rsIdxStr, rIdxStr] = key.split(":");
    const rsIdx = Number(rsIdxStr);
    const rIdx = Number(rIdxStr);
    const rs = program.rulesets[rsIdx];
    const rule = rs.rules[rIdx];
    const capturesList = rule.match(sent, sourceText, idx);
    for (const captures of capturesList) {
      hits.push({ ruleId: rule.id, rulesetId: rs.id, captures });
    }
  }
  return hits;
}
function matchDoc(program, doc, sourceText, opts = {}) {
  const hits = [];
  for (const sent of doc.sentences) {
    hits.push(...matchSentence(program, sent, sourceText, opts));
  }
  return hits;
}

// packages/grammar/src/program.ts
class GrammarEngine {
  client;
  program;
  ruleSpecs;
  constructor(client, program, ruleSpecs) {
    this.client = client;
    this.program = program;
    this.ruleSpecs = ruleSpecs;
  }
  static async create(rulesets, opts = {}) {
    const program = buildProgram(rulesets);
    const specsMap = new Map;
    for (const rs of rulesets) {
      for (const r of rs.rules) {
        specsMap.set(r.id, r);
      }
    }
    const client = new GinzaClient(opts.ginza);
    await client.start();
    return new GrammarEngine(client, program, specsMap);
  }
  async close() {
    await this.client.stop();
  }
  getRulesetIds() {
    return this.program.rulesets.map((rs) => rs.id);
  }
  getRuleIds() {
    return this.program.rulesets.flatMap((rs) => rs.rules.map((r) => r.id));
  }
  async match(text2, opts = {}) {
    const [doc] = await this.client.analyze([text2]);
    if (!doc)
      return [];
    return matchDoc(this.program, doc, text2, opts);
  }
  async analyze(text2) {
    const [doc] = await this.client.analyze([text2]);
    return doc ?? null;
  }
  matchDoc(doc, sourceText, opts = {}) {
    return matchDoc(this.program, doc, sourceText, opts);
  }
  async explainMatch(text2, ruleId) {
    const spec = this.ruleSpecs.get(ruleId);
    if (!spec) {
      return { matched: false, reason: `Unknown rule: ${ruleId}`, partialBinding: {}, triedCandidates: {} };
    }
    const [doc] = await this.client.analyze([text2]);
    if (!doc || doc.sentences.length === 0) {
      return { matched: false, reason: "No sentences parsed", partialBinding: {}, triedCandidates: {} };
    }
    return explainMatch(spec, doc.sentences[0], text2);
  }
}

// packages/grammar/src/rules/bunpro/_test/engine.ts
globalThis.__bunproTestEngineRefCount ??= 0;
async function getSharedEngine(rulesets) {
  if (!globalThis.__bunproTestEnginePromise) {
    globalThis.__bunproTestEnginePromise = GrammarEngine.create(rulesets, {
      ginza: { python: "python3" }
    });
    globalThis.__bunproTestEngine = await globalThis.__bunproTestEnginePromise;
  }
  return globalThis.__bunproTestEnginePromise;
}
function useSharedEngine(rulesets) {
  let engine;
  beforeAll(async () => {
    engine = await getSharedEngine(rulesets);
    globalThis.__bunproTestEngineRefCount++;
  });
  afterAll(async () => {
    globalThis.__bunproTestEngineRefCount--;
    if (globalThis.__bunproTestEngineRefCount === 0 && globalThis.__bunproTestEngine) {
      await globalThis.__bunproTestEngine.close();
      globalThis.__bunproTestEngine = undefined;
      globalThis.__bunproTestEnginePromise = undefined;
    }
  });
  return {
    get: () => engine
  };
}

// packages/grammar/src/rules/bunpro/_test/helpers.ts
import { describe, it, expect } from "bun:test";
import { existsSync as existsSync2 } from "node:fs";
import { join as join3 } from "node:path";
import { fileURLToPath as fileURLToPath2 } from "node:url";

// packages/grammar/src/data/bunpro/loader.ts
import { readFileSync, readdirSync } from "node:fs";
var BunproSchema = exports_external.object({
  data: exports_external.object({
    attributes: exports_external.object({
      slug: exports_external.string(),
      title: exports_external.string().optional(),
      meaning: exports_external.string().optional()
    })
  }),
  included: exports_external.array(exports_external.any()).optional()
});
function cleanHtml(text2) {
  return text2.replace(/<[^>]+>/g, "").replace(/（[^）]*）/g, "").replace(/\([^)]*\)/g, "").replace(/\s+/g, " ").trim();
}
function extractSentence(content, answer) {
  return cleanHtml(content.replaceAll("____", answer));
}
function isNonTrivialSlug(slug) {
  const bad = new Set(["は", "が", "を", "に", "で", "と", "の", "も", "へ", "や", "か"]);
  if (bad.has(slug))
    return false;
  if (slug.length <= 1)
    return false;
  return true;
}
function loadBunproGrammarItemWithOptions(filePath, level, opts) {
  const raw = readFileSync(filePath, "utf8");
  const parsed = BunproSchema.safeParse(JSON.parse(raw));
  if (!parsed.success)
    return null;
  const attrs = parsed.data.data.attributes;
  const slug = attrs.slug;
  if (!opts.allowTrivialSlug && !isNonTrivialSlug(slug))
    return null;
  const answerForms = new Set;
  const sentences = [];
  for (const item of parsed.data.included ?? []) {
    if (item?.type !== "study_question")
      continue;
    const a = item?.attributes ?? {};
    const content = typeof a.content === "string" ? a.content : "";
    const answer = typeof a.answer === "string" ? a.answer : "";
    const alternates = Array.isArray(a.alternate_grammar) ? a.alternate_grammar : [];
    if (answer)
      answerForms.add(cleanHtml(answer));
    for (const alt of alternates) {
      if (typeof alt === "string" && alt.trim())
        answerForms.add(cleanHtml(alt));
    }
    if (content && answer) {
      const sentence = extractSentence(content, answer);
      if (sentence.length > 3)
        sentences.push({ sentence, answer: cleanHtml(answer) });
    }
  }
  const answerFormsList = [...answerForms].filter(Boolean);
  if (answerFormsList.length === 0 || sentences.length === 0)
    return null;
  return {
    id: slug,
    level,
    title: attrs.title,
    meaning: attrs.meaning,
    answerForms: answerFormsList,
    sentences: sentences.slice(0, 20)
  };
}

// packages/grammar/src/rules/bunpro/_test/helpers.ts
var DATA_ROOT = fileURLToPath2(new URL("../../../../data/bunpro", import.meta.url));
function loadTestItem(ruleId, level) {
  const filePath = join3(DATA_ROOT, level, `${ruleId}.json`);
  if (!existsSync2(filePath)) {
    throw new Error(`Missing test data: ${filePath} for rule '${ruleId}'`);
  }
  const item = loadBunproGrammarItemWithOptions(filePath, level, { allowTrivialSlug: true });
  if (!item) {
    throw new Error(`Failed to load bunpro item: ${filePath}`);
  }
  return item;
}
function describeRule(rule, level, rulesetId, getEngine, opts = {}) {
  describe(rule.id, () => {
    const item = loadTestItem(rule.id, level);
    describe("positives", () => {
      const { skipPositives = [] } = opts;
      for (const { sentence } of item.sentences) {
        if (skipPositives.includes(sentence)) {
          it.skip(`✓ ${sentence.slice(0, 40)} (GiNZA limitation)`, () => {});
          continue;
        }
        it(`✓ ${sentence.slice(0, 40)}`, async () => {
          const engine = getEngine();
          const hits = await engine.match(sentence);
          const hit = hits.find((h) => h.ruleId === rule.id);
          if (!hit) {
            const explain = await engine.explainMatch(sentence, rule.id);
            if (!explain.matched) {
              console.log(`
❌ Rule '${rule.id}' failed on: ${sentence}`);
              console.log(`   Reason: ${explain.reason}`);
              if (explain.failedClause) {
                console.log(`   Failed clause: ${explain.failedClause.kind}`);
              }
              if (explain.partialBinding && Object.keys(explain.partialBinding).length > 0) {
                console.log(`   Partial bindings: ${JSON.stringify(explain.partialBinding)}`);
              }
            }
          }
          expect(hit).toBeDefined();
          expect(hit.rulesetId).toBe(rulesetId);
          for (const [, cap] of Object.entries(hit.captures)) {
            expect(typeof cap.start).toBe("number");
            expect(typeof cap.end).toBe("number");
            expect(cap.text.length).toBeGreaterThan(0);
          }
        });
      }
    });
    const { negatives } = opts;
    if (negatives && negatives.length > 0) {
      describe("negatives", () => {
        for (const sentence of negatives) {
          it(`✗ ${sentence.slice(0, 40)}`, async () => {
            const engine = getEngine();
            const hits = await engine.match(sentence);
            const hit = hits.find((h) => h.ruleId === rule.id);
            if (hit) {
              console.log(`
❌ FALSE POSITIVE: Rule '${rule.id}' matched: ${sentence}`);
              console.log(`   Captured: ${JSON.stringify(hit.captures)}`);
            }
            expect(hit).toBeUndefined();
          });
        }
      });
    }
  });
}

/* packages/grammar/src/rules/bunpro/jlpt5/verb-て-b.ts */
var verb_b_default = linguisticRule("verb-て-b", (r) => {
  const verb = r.verb({
    dep: "advcl"
  }, "verb");
  const te = r.tok({
    text: "て",
    lemma: "て",
    pos: "SCONJ",
    dep: "mark"
  }, "te");
  r.headChild(verb, te, "mark");
  r.inOrder(verb, te, 1);
  r.captureSpan("verb-te", verb, te);
});

/* packages/grammar/src/rules/bunpro/jlpt5/adjective-て-b.ts */
var adjective_b_default = linguisticRule("adjective-て-b", (r) => {
  r.either((r1) => {
    const iAdj = r1.tok({
      conjugationClass: "形容詞"
    }, "iAdj");
    const te = r1.tok({
      text: "て",
      pos: "SCONJ"
    }, "te");
    r1.headChild(iAdj, te, "mark");
    r1.inOrder(iAdj, te, 1);
    r1.captureSpan("て-form", iAdj, te);
  }, (r2) => {
    const naAdj = r2.tok({
      pos: "ADJ",
      depOneOf: ["advcl", "acl"]
    }, "naAdj");
    const de = r2.tok({
      text: "で",
      pos: "AUX",
      dep: "aux"
    }, "de");
    r2.headChild(naAdj, de);
    r2.inOrder(naAdj, de, 1);
    r2.captureSpan("で-form", naAdj, de);
  }, (r3) => {
    const naAdj = r3.tok({
      pos: "ADJ",
      depOneOf: ["advcl", "obl"]
    }, "naAdj");
    const de = r3.tok({
      text: "で",
      pos: "ADP",
      lemma: "だ"
    }, "de");
    r3.headChild(naAdj, de, "case");
    r3.inOrder(naAdj, de, 1);
    r3.captureSpan("で-form", naAdj, de);
  }, (r4) => {
    const noun = r4.tok({
      posOneOf: ["NOUN", "PROPN"]
    }, "noun");
    const de = r4.tok({
      text: "で",
      pos: "ADP",
      lemma: "だ"
    }, "de");
    r4.headChild(noun, de, "case");
    r4.inOrder(noun, de, 1);
    r4.captureSpan("で-form", noun, de);
  });
});

/* packages/grammar/src/rules/bunpro/jlpt5/い-adjectives.ts */
var い_adjectives_default = linguisticRule("い-adjectives", (r) => {
  const adj = r.tok({
    lemmaOneOf: [
      "さむい",
      "あつい",
      "たのしい",
      "たかい",
      "おいしい",
      "かわいい",
      "おおきい",
      "あたたかい",
      "ちかい",
      "ふるい",
      "あたらしい",
      "むずかしい",
      "やすい",
      "とおい",
      "ながい",
      "はやい",
      "こわい",
      "おもしろい",
      "おとなしい",
      "つめたい",
      "まずい",
      "うつくしい",
      "い",
      "うつくしい",
      "せまい",
      "温い",
      "狭い"
    ],
    tag: "形容詞-一般"
  }, "adj");
  r.capture(adj);
});

/* packages/grammar/src/rules/bunpro/jlpt5/-んです-のです.ts */
var ___default = linguisticRule("-んです-のです", (r) => {
  const n = r.tok({ textOneOf: ["ん", "の"] }, "n");
  const desu = r.aux({ lemma: "です" }, "desu");
  r.inOrder(n, desu, 2);
  r.captureSpan("んです", n, desu);
});

/* packages/grammar/src/rules/bunpro/jlpt5/つもりだ.ts */
var つ__default = linguisticRule("つもりだ", (r) => {
  const tsumori = r.tok({ lemma: "つもり", pos: "NOUN" }, "tsumori");
  const v = r.verb({}, "v");
  r.headChild(tsumori, v, "acl");
  r.captureAs("verb", v);
  r.capture(tsumori);
});

/* packages/grammar/src/rules/bunpro/jlpt5/か.ts */
var か_default = linguisticRule("か", (r) => {
  r.either((b) => {
    const ka = b.particle("か", "ka");
    b.capture(ka);
  }, (b) => {
    const ka = b.tok({ text: "か", pos: "ADP", dep: "case" }, "ka");
    b.capture(ka);
  });
});

/* packages/grammar/src/rules/bunpro/jlpt5/か-or.ts */
var か_or_default = linguisticRule("か-or", (r) => {
  r.either((branch1) => {
    const ka = branch1.tok({ text: "か", pos: "ADP", dep: "case" }, "ka");
    const nextWord = branch1.tok({ posOneOf: ["NOUN", "VERB", "ADJ", "PRON"] });
    branch1.inOrder(ka, nextWord, 1);
    branch1.capture(ka);
  }, (branch2) => {
    const ka = branch2.tok({ text: "か", pos: "PART", dep: "mark" }, "ka");
    const punct = branch2.tok({ pos: "PUNCT" });
    branch2.inOrder(ka, punct, 5);
    branch2.capture(ka);
  });
});

/* packages/grammar/src/rules/bunpro/jlpt5/から-because.ts */
var か_because_default = linguisticRule("から-because", (r) => {
  r.either((r1) => {
    const noun = r1.tok({ posOneOf: ["NOUN", "PROPN"] }, "noun");
    const da = r1.tok({ text: "だ", pos: "AUX", depOneOf: ["cop", "aux"] }, "da");
    const kara = r1.particle("から", "kara", { pos: "SCONJ", dep: "mark" });
    r1.inOrder(noun, da, 1);
    r1.inOrder(da, kara, 1);
    r1.headChild(noun, da);
    r1.headChild(noun, kara);
    r1.captureSpan("から-because", noun, kara);
  }, (r2) => {
    const naAdj = r2.adj({}, "naAdj");
    const desu = r2.tok({ textOneOf: ["だ", "です"], pos: "AUX" }, "desu");
    const kara = r2.particle("から", "kara", { pos: "SCONJ", dep: "mark" });
    r2.inOrder(naAdj, desu, 1);
    r2.inOrder(desu, kara, 1);
    r2.headChild(naAdj, desu);
    r2.headChild(naAdj, kara);
    r2.captureSpan("から-because", naAdj, kara);
  }, (r3) => {
    const iAdj = r3.tok({ pos: "ADJ", conjugationClass: "形容詞" }, "iAdj");
    const kara = r3.particle("から", "kara", { pos: "SCONJ", dep: "mark" });
    r3.inOrder(iAdj, kara, 1);
    r3.headChild(iAdj, kara);
    r3.captureSpan("から-because", iAdj, kara);
  }, (r4) => {
    const verb = r4.verb({}, "verb");
    const aux = r4.tok({ lemmaOneOf: ["た", "し"], pos: "AUX" }, "aux");
    const kara = r4.particle("から", "kara", { pos: "SCONJ", dep: "mark" });
    r4.inOrder(verb, aux, 2);
    r4.inOrder(aux, kara, 1);
    r4.auxOf(verb, aux);
    r4.headChild(verb, kara);
    r4.captureSpan("から-because", verb, kara);
  }, (r5) => {
    const verb = r5.verb({}, "verb");
    const kara = r5.particle("から", "kara", { pos: "SCONJ", dep: "mark" });
    r5.inOrder(verb, kara, 1);
    r5.headChild(verb, kara);
    r5.captureSpan("から-because", verb, kara);
  });
});

/* packages/grammar/src/rules/bunpro/jlpt5/から.ts */
var か__default = linguisticRule("から", (r) => {
  const kara = r.particle("から", "kara", { dep: "case" });
  const noun = r.tok({ posOneOf: ["NOUN", "PRON", "PROPN", "DET", "NUM", "ADV"] }, "noun");
  r.caseMarker(noun, kara);
  r.capture(kara);
});

/* packages/grammar/src/rules/bunpro/jlpt5/が.ts */
var が_default = linguisticRule("が", (r) => {
  const ga = r.particle("が", "ga", { dep: "case" });
  const noun = r.tok({ posOneOf: ["NOUN", "PRON", "DET", "NUM"] }, "noun");
  r.caseMarker(noun, ga);
  r.not((nr) => {
    const verb = nr.tok({ posOneOf: ["VERB", "AUX"] }, "verb");
    nr.headChild(verb, ga);
  });
  r.capture(ga);
});

/* packages/grammar/src/rules/bunpro/jlpt5/が-but.ts */
var が_but_default = linguisticRule("が-but", (r) => {
  const ga = r.particle("が", "ga", { pos: "SCONJ", dep: "mark" });
  r.capture(ga);
});

/* packages/grammar/src/rules/bunpro/jlpt5/がある.ts */
var が__default = linguisticRule("がある", (r) => {
  r.either((b) => {
    const ga = b.particle("が", "ga");
    const aru = b.tok({ lemma: "ある", pos: "VERB" }, "aru");
    b.inOrder(ga, aru, 1);
    b.captureSpan("がある", ga, aru);
  }, (b) => {
    const ga = b.particle("が", "ga");
    const aru = b.tok({ lemma: "ある", pos: "VERB", inflectionForm: "連用形-一般" }, "aru");
    const masu = b.tok({ lemma: "ます", pos: "AUX" }, "masu");
    b.auxOf(aru, masu);
    b.inOrder(ga, aru, 1);
    b.captureSpan("がある", ga, masu);
  }, (b) => {
    const ga = b.particle("が", "ga");
    const nai = b.tok({ lemma: "ない", pos: "ADJ" }, "nai");
    b.inOrder(ga, nai, 1);
    b.captureSpan("がある", ga, nai);
  }, (b) => {
    const ga = b.particle("が", "ga");
    const aru = b.tok({ lemma: "ある", pos: "VERB", inflectionForm: "未然形-一般" }, "aru");
    const masen = b.tok({ lemma: "ません", pos: "AUX" }, "masen");
    b.auxOf(aru, masen);
    b.inOrder(ga, aru, 1);
    b.captureSpan("がある", ga, masen);
  });
});

/* packages/grammar/src/rules/bunpro/jlpt5/がいる.ts */
var が__default2 = linguisticRule("がいる", (r) => {
  r.either((b) => {
    const ga = b.particle("が", "ga");
    const iru = b.tok({ lemma: "いる", pos: "VERB" }, "iru");
    b.inOrder(ga, iru, 1);
    b.captureSpan("がいる", ga, iru);
  }, (b) => {
    const ga = b.particle("が", "ga");
    const iru = b.tok({ lemma: "いる", pos: "VERB" }, "iru");
    const masu = b.tok({ lemma: "ます", pos: "AUX" }, "masu");
    b.auxOf(iru, masu);
    b.inOrder(ga, iru, 1);
    b.captureSpan("がいる", ga, masu);
  });
});

/* packages/grammar/src/rules/bunpro/jlpt5/くる.ts */
var く__default = linguisticRule("くる", (r) => {
  const kuru = r.verb({ lemma: "くる", conjugationClass: "カ行変格" }, "kuru");
  r.capture(kuru);
});

/* packages/grammar/src/rules/bunpro/jlpt5/して.ts */
var し__default = linguisticRule("して", (r) => {
  const verb = r.verb({}, "verb");
  const shi = r.aux({
    text: "し",
    lemma: "する"
  }, "shi");
  const te = r.tok({
    text: "て",
    lemma: "て",
    pos: "SCONJ"
  }, "te");
  r.auxOf(verb, shi);
  r.headChild(verb, te, "mark");
  r.inOrder(shi, te, 1);
  r.captureSpan("して", shi, te);
});

/* packages/grammar/src/rules/bunpro/jlpt5/てから.ts */
var て__default = linguisticRule("てから", (r) => {
  const te = r.tok({ textOneOf: ["て", "で"], pos: "SCONJ", dep: "mark" }, "te");
  const kara = r.particle("から", "kara", { pos: "ADP", dep: "case", lemma: "から" });
  r.inOrder(te, kara, 1);
  r.captureSpan("てから", te, kara);
});

/* packages/grammar/src/rules/bunpro/jlpt5/だ.ts */
var だ_default = linguisticRule("だ", (r) => {
  r.either((branch) => {
    const head = branch.tok({ posOneOf: ["NOUN", "PRON", "DET", "NUM"] }, "head");
    const da = branch.aux({ lemma: "だ", dep: "cop" }, "da");
    branch.copulaOf(head, da);
    branch.capture(da);
  }, (branch) => {
    const naAdj = branch.adj({}, "naAdj");
    const da = branch.aux({ lemma: "だ", dep: "aux" }, "da");
    branch.auxOf(naAdj, da);
    branch.not((nr) => {
      nr.adj({ conjugationClass: "形容詞" }, "naAdj");
    });
    branch.capture(da);
  });
});

/* packages/grammar/src/rules/bunpro/jlpt5/です.ts */
var で__default = linguisticRule("です", (r) => {
  r.either((r1) => {
    const noun = r1.tok({ posOneOf: ["NOUN", "PROPN", "PRON", "NUM"] }, "noun");
    const desu = r1.aux({ text: "です", dep: "cop" }, "desu");
    r1.copulaOf(noun, desu);
    r1.capture(desu);
  }, (r2) => {
    const adj = r2.adj({}, "adj");
    const desu = r2.aux({ text: "です", dep: "aux" }, "desu");
    r2.auxOf(adj, desu);
    r2.capture(desu);
  });
});

/* packages/grammar/src/rules/bunpro/jlpt5/じゃない.ts */
var じ__default = linguisticRule("じゃない", (r) => {
  r.either((branch) => {
    const head = branch.tok({ posOneOf: ["NOUN", "PRON", "DET", "NUM"] }, "head");
    const ja = branch.aux({ text: "じゃ", lemma: "だ", dep: "cop" }, "ja");
    const nai = branch.aux({ lemma: "ない", dep: "fixed" }, "nai");
    branch.copulaOf(head, ja);
    branch.headChild(ja, nai, "fixed");
    branch.captureSpan("じゃない", ja, nai);
  }, (branch) => {
    const naAdj = branch.adj({}, "naAdj");
    const ja = branch.aux({ text: "じゃ", lemma: "だ", dep: "aux" }, "ja");
    const nai = branch.aux({ lemma: "ない", dep: "fixed" }, "nai");
    branch.auxOf(naAdj, ja);
    branch.headChild(ja, nai, "fixed");
    branch.not((nr) => {
      nr.adj({ conjugationClass: "形容詞" }, "naAdj");
    });
    branch.captureSpan("じゃない", ja, nai);
  });
});

/* packages/grammar/src/rules/bunpro/jlpt5/は.ts */
var は_default = linguisticRule("は", (r) => {
  const wa = r.particle("は", "wa", { dep: "case" });
  const noun = r.tok({ posOneOf: ["NOUN", "PRON", "DET", "NUM", "PROPN", "ADJ"] }, "noun");
  r.caseMarker(noun, wa);
  r.capture(wa);
});

/* packages/grammar/src/rules/bunpro/jlpt5/を.ts */
var を_default = linguisticRule("を", (r) => {
  const wo = r.particle("を", "wo", { dep: "case" });
  const noun = r.tok({ posOneOf: ["NOUN", "PROPN", "PRON", "DET", "NUM"] }, "noun");
  r.caseMarker(noun, wo);
  r.capture(wo);
});

/* packages/grammar/src/rules/bunpro/jlpt5/で.ts */
var で_default = linguisticRule("で", (r) => {
  const de = r.particle("で", "de", { dep: "case" });
  const noun = r.tok({ posOneOf: ["NOUN", "PRON", "DET", "NUM"] }, "noun");
  r.caseMarker(noun, de);
  r.capture(de);
});

/* packages/grammar/src/rules/bunpro/jlpt5/に.ts */
var に_default = linguisticRule("に", (r) => {
  const ni = r.particle("に", "ni", { depOneOf: ["case", "obl"] });
  const noun = r.tok({ posOneOf: ["NOUN", "PRON", "DET", "NUM"] }, "noun");
  r.caseMarker(noun, ni);
  r.capture(ni);
});

/* packages/grammar/src/rules/bunpro/jlpt5/へ.ts */
var へ_default = linguisticRule("へ", (r) => {
  const he = r.particle("へ", "he", { dep: "case" });
  const noun = r.tok({ posOneOf: ["NOUN", "PRON", "PROPN", "DET", "NUM"] }, "noun");
  r.caseMarker(noun, he);
  r.capture(he);
});

/* packages/grammar/src/rules/bunpro/jlpt5/と.ts */
var と_default = linguisticRule("と", (r) => {
  const to = r.particle("と", "to", { pos: "ADP", dep: "case" });
  r.either((r1) => {
    const quoted = r1.tok({
      depOneOf: ["ccomp", "advcl", "acl", "root"]
    }, "quoted");
    r1.headChild(quoted, to);
    r1.capture(to);
  }, (r2) => {
    const quoted = r2.tok({
      dep: "obl",
      posOneOf: ["ADJ", "VERB", "AUX"]
    }, "quoted");
    r2.headChild(quoted, to);
    r2.capture(to);
  });
});

/* packages/grammar/src/rules/bunpro/jlpt5/よ.ts */
var よ_default = linguisticRule("よ", (r) => {
  r.either((branch1) => {
    const yo = branch1.tok({ text: "よ", pos: "PART", dep: "mark" }, "yo");
    const punct = branch1.tok({ pos: "PUNCT" });
    branch1.inOrder(yo, punct, 1);
    branch1.capture(yo);
  }, (branch2) => {
    const yo = branch2.tok({ text: "よ", pos: "PART", dep: "mark" }, "yo");
    const particle = branch2.tok({ text: "ね", pos: "PART" });
    branch2.inOrder(yo, particle, 1);
    branch2.capture(yo);
  });
});

/* packages/grammar/src/rules/bunpro/jlpt5/ね.ts */
var ね_default = linguisticRule("ね", (r) => {
  const ne = r.tok({ text: "ね", pos: "PART" }, "ne");
  r.capture(ne);
});

/* packages/grammar/src/rules/bunpro/jlpt5/あげる.ts */
var あ__default = linguisticRule("あげる", (r) => {
  const ageru = r.verb({
    lemma: "あげる"
  }, "ageru");
  r.capture(ageru);
});

/* packages/grammar/src/rules/bunpro/jlpt5/いい.ts */
var い__default = linguisticRule("いい", (r) => {
  const ii = r.adj({
    lemmaOneOf: ["いい", "よい"],
    tag: "形容詞-非自立可能"
  }, "ii");
  r.capture(ii);
});

/* packages/grammar/src/rules/bunpro/jlpt5/う-Verbs.ts */
var う_Verbs_default = linguisticRule("う-Verbs", (r) => {
  r.either((b) => {
    const verb = b.verb({
      lemmaOneOf: ["聞く", "行く", "泳ぐ"],
      conjugationClass: "五段-カ行"
    }, "uVerb");
    b.capture(verb);
  }, (b) => {
    const verb = b.verb({
      lemmaOneOf: ["泳ぐ"],
      conjugationClass: "五段-ガ行"
    }, "uVerb");
    b.capture(verb);
  }, (b) => {
    const verb = b.verb({
      lemmaOneOf: ["話す"],
      conjugationClass: "五段-サ行"
    }, "uVerb");
    b.capture(verb);
  }, (b) => {
    const verb = b.verb({
      lemmaOneOf: ["打つ"],
      conjugationClass: "五段-タ行"
    }, "uVerb");
    b.capture(verb);
  }, (b) => {
    const verb = b.verb({
      lemmaOneOf: ["死ぬ"],
      conjugationClass: "五段-ナ行"
    }, "uVerb");
    b.capture(verb);
  }, (b) => {
    const verb = b.verb({
      lemmaOneOf: ["飛ぶ"],
      conjugationClass: "五段-バ行"
    }, "uVerb");
    b.capture(verb);
  }, (b) => {
    const verb = b.verb({
      lemmaOneOf: ["飲む"],
      conjugationClass: "五段-マ行"
    }, "uVerb");
    b.capture(verb);
  }, (b) => {
    const verb = b.verb({
      lemmaOneOf: ["会う", "座る", "帰る", "歩く"],
      conjugationClass: "五段-ラ行"
    }, "uVerb");
    b.capture(verb);
  }, (b) => {
    const verb = b.verb({
      lemma: "言う",
      conjugationClass: "五段-ワア行"
    }, "uVerb");
    b.capture(verb);
  });
});

/* packages/grammar/src/rules/bunpro/jlpt5/うverb--ない.ts */
var うverb__default = linguisticRule("うverb--ない", (r) => {
  const godanClasses = [
    "五段-カ行",
    "五段-ガ行",
    "五段-サ行",
    "五段-タ行",
    "五段-ナ行",
    "五段-バ行",
    "五段-マ行",
    "五段-ラ行",
    "五段-ワア行"
  ];
  const branches = [];
  for (const conjugationClass2 of godanClasses) {
    branches.push((b) => {
      const verb = b.verb({
        conjugationClass: conjugationClass2,
        inflectionForm: "未然形-一般"
      }, "verb");
      const nai = b.aux({
        lemma: "ない",
        conjugationClass: "助動詞-ナイ"
      }, "nai");
      b.auxOf(verb, nai);
      b.captureSpan("うverb--ない", verb, nai);
    });
    branches.push((b) => {
      const verb = b.verb({
        conjugationClass: conjugationClass2,
        inflectionForm: "連用形-一般"
      }, "verb");
      const mase = b.aux({
        lemma: "ます",
        inflectionForm: "未然形-一般"
      }, "mase");
      b.auxOf(verb, mase);
      b.captureSpan("うverb--ない", verb, mase);
    });
  }
  r.either(...branches);
});

/* packages/grammar/src/rules/bunpro/jlpt5/う-verb-past.ts */
var う_verb_past_default = linguisticRule("う-verb-past", (r) => {
  const godanClasses = [
    "五段-カ行",
    "五段-ガ行",
    "五段-サ行",
    "五段-タ行",
    "五段-ナ行",
    "五段-バ行",
    "五段-マ行",
    "五段-ラ行",
    "五段-ワア行"
  ];
  r.either(...godanClasses.map((cc) => (b) => {
    const uVerb = b.verb({ conjugationClass: cc }, "verb");
    const pastAux = b.aux({
      lemmaOneOf: ["た", "だ"],
      conjugationClass: "助動詞-タ"
    }, "past");
    b.auxOf(uVerb, pastAux);
    b.captureSpan("match", uVerb, pastAux);
  }));
});

/* packages/grammar/src/rules/bunpro/jlpt5/う-verb-neg-past.ts */
var う_verb_neg_past_default = linguisticRule("う-verb-neg-past", (r) => {
  r.either((b) => {
    const verb = b.verb({
      conjugationClassOneOf: [
        "五段-カ行",
        "五段-ガ行",
        "五段-サ行",
        "五段-タ行",
        "五段-ナ行",
        "五段-バ行",
        "五段-マ行",
        "五段-ラ行",
        "五段-ワア行"
      ],
      inflectionForm: "未然形-一般"
    }, "verb");
    const nakatta = b.aux({
      lemma: "ない",
      inflectionForm: "連用形-促音便"
    }, "nakatta");
    b.auxOf(verb, nakatta);
    b.captureSpan("う-verb-neg-past", verb, nakatta);
  }, (b) => {
    const verb = b.verb({
      conjugationClassOneOf: [
        "五段-カ行",
        "五段-ガ行",
        "五段-サ行",
        "五段-タ行",
        "五段-ナ行",
        "五段-バ行",
        "五段-マ行",
        "五段-ラ行",
        "五段-ワア行"
      ],
      inflectionForm: "連用形-一般"
    }, "verb");
    const masenDesita = b.tok({
      lemma: "ます",
      pos: "AUX",
      inflectionForm: "未然形-一般"
    }, "masenDesita");
    b.auxOf(verb, masenDesita);
    b.captureSpan("う-verb-neg-past", verb, masenDesita);
  });
});

/* packages/grammar/src/rules/bunpro/jlpt5/きらい.ts */
var き__default = linguisticRule("きらい", (r) => {
  r.either((branch) => {
    const kirai = branch.tok({
      lemmaOneOf: ["きらい", "嫌い"],
      posOneOf: ["NOUN", "ADJ", "VERB"]
    }, "kirai");
    branch.capture(kirai);
  }, (branch) => {
    const kirai = branch.adj({ lemma: "大嫌い" }, "kirai");
    branch.capture(kirai);
  }, (branch) => {
    const kirai = branch.tok({
      lemma: "大きらい",
      posOneOf: ["NOUN", "ADJ", "VERB"]
    }, "kirai");
    branch.capture(kirai);
  });
});

/* packages/grammar/src/rules/bunpro/jlpt5/くらい1.ts */
var く_1_default = linguisticRule("くらい1", (r) => {
  r.either((b) => {
    const kurai = b.particle("くらい", "kurai");
    b.capture(kurai);
  }, (b) => {
    const gurai = b.particle("ぐらい", "gurai");
    b.capture(gurai);
  });
});

/* packages/grammar/src/rules/bunpro/jlpt5/くれる.ts */
var く__default2 = linguisticRule("くれる", (r) => {
  r.either((b) => {
    const kureru = b.verb({ lemma: "くれる" }, "kureru");
    b.capture(kureru);
  }, (b) => {
    const kureru = b.verb({ lemma: "呉れる" }, "kureru");
    b.capture(kureru);
  });
});

/* packages/grammar/src/rules/bunpro/jlpt5/けっこう.ts */
var け__default = linguisticRule("けっこう", (r) => {
  r.either((branch) => {
    const kekkou = branch.adv({ lemmaOneOf: ["けっこう", "結構"], dep: "advmod" }, "kekkou");
    branch.capture(kekkou);
  }, (branch) => {
    const kekkou = branch.adv({ lemmaOneOf: ["けっこう", "結構"], dep: "root" }, "kekkou");
    branch.capture(kekkou);
  }, (branch) => {
    const kekkou = branch.adj({ lemmaOneOf: ["けっこう", "結構"], dep: "root" }, "kekkou");
    branch.capture(kekkou);
  });
});

/* packages/grammar/src/rules/bunpro/jlpt5/けど-だけど.ts */
var け__default2 = linguisticRule("けど-だけど", (r) => {
  r.either((r1) => {
    const head = r1.tok({ posOneOf: ["NOUN", "PROPN", "ADJ", "ADV", "VERB"] }, "head");
    const da = r1.tok({ text: "だ", pos: "AUX" }, "da");
    const kedo = r1.particle("けど", "kedo", { depOneOf: ["dep", "cc", "mark"] });
    r1.inOrder(head, da, 2);
    r1.inOrder(da, kedo, 1);
    r1.headChild(head, kedo);
    r1.captureSpan("けど-だけど", head, kedo);
  }, (r2) => {
    const head = r2.tok({ posOneOf: ["VERB", "ADJ"] }, "head");
    const kedo = r2.particle("けど", "kedo", { depOneOf: ["dep", "cc", "mark"] });
    r2.inOrder(head, kedo, 1);
    r2.headChild(head, kedo);
    r2.captureSpan("けど-だけど", head, kedo);
  }, (r3) => {
    const head = r3.tok({ posOneOf: ["VERB", "ADJ", "NOUN", "PROPN"] }, "head");
    const keredo = r3.particle("けれど", "keredo", { depOneOf: ["dep", "cc", "mark"] });
    r3.inOrder(head, keredo, 1);
    r3.headChild(head, keredo);
    r3.captureSpan("けど-だけど", head, keredo);
  }, (r4) => {
    const head = r4.tok({ posOneOf: ["VERB", "ADJ", "NOUN", "PROPN"] }, "head");
    const keredomo = r4.particle("けれども", "keredomo", { depOneOf: ["dep", "cc", "mark"] });
    r4.inOrder(head, keredomo, 1);
    r4.headChild(head, keredomo);
    r4.captureSpan("けど-だけど", head, keredomo);
  });
});

/* packages/grammar/src/rules/bunpro/jlpt5/けれども.ts */
var け__default3 = linguisticRule("けれども", (r) => {
  r.either((b) => {
    const da = b.tok({ lemma: "だ" }, "da");
    const keredomo = b.tok({
      textOneOf: ["けれども", "けれど"],
      depOneOf: ["dep", "cc", "mark"]
    }, "keredomo");
    b.inOrder(da, keredomo, 1);
    b.captureSpan("だけれども", da, keredomo);
  }, (b) => {
    const n = b.aux({ lemma: "ん" }, "n");
    const keredomo = b.tok({
      textOneOf: ["けれども", "けれど"],
      depOneOf: ["dep", "cc", "mark"]
    }, "keredomo");
    b.inOrder(n, keredomo, 1);
    b.captureSpan("んだけれども", n, keredomo);
  }, (b) => {
    const keredomo = b.tok({
      textOneOf: ["けれども", "けれど"],
      depOneOf: ["dep", "cc", "mark"]
    }, "keredomo");
    b.capture(keredomo);
  }, (b) => {
    const kedo = b.tok({ text: "けど" }, "kedo");
    const mo = b.particle("も", "mo");
    b.inOrder(kedo, mo, 1);
    b.captureSpan("けども", kedo, mo);
  });
});

/* packages/grammar/src/rules/bunpro/jlpt5/ここ.ts */
var こ__default = linguisticRule("ここ", (r) => {
  r.either((branch) => {
    const koko = branch.tok({ lemma: "ここ", pos: "PRON" }, "koko");
    branch.capture(koko);
  }, (branch) => {
    const koko = branch.tok({ lemma: "此処", pos: "PRON" }, "koko");
    branch.capture(koko);
  });
});

/* packages/grammar/src/rules/bunpro/jlpt5/この.ts */
var こ__default2 = linguisticRule("この", (r) => {
  r.either((branch) => {
    const kono = branch.tok({ lemma: "この", pos: "DET" }, "kono");
    const noun = branch.noun({}, "noun");
    branch.headChild(noun, kono, "det");
    branch.capture(kono);
  }, (branch) => {
    const kono = branch.tok({ textOneOf: ["此の", "この"], pos: "DET" }, "kono");
    const noun = branch.noun({}, "noun");
    branch.headChild(noun, kono, "det");
    branch.capture(kono);
  });
});

/* packages/grammar/src/rules/bunpro/jlpt5/これ.ts */
var こ__default3 = linguisticRule("これ", (r) => {
  r.either((b) => {
    const kore = b.tok({ lemma: "これ", posOneOf: ["PRON", "NOUN"] }, "kore");
    b.capture(kore);
  }, (b) => {
    const koreKanji = b.tok({ text: "此れ", posOneOf: ["PRON", "NOUN"] }, "kore");
    b.capture(koreKanji);
  });
});

// packages/grammar/src/rules/bunpro/jlpt5/index.ts
var BUNPRO_JLPT5 = {
  id: "bunpro.jlpt5",
  rules: [
    adjective_b_default,
    あ__default,
    が__default,
    が_but_default,
    が_default,
    が__default2,
    い__default,
    い_adjectives_default,
    か_default,
    か_or_default,
    か__default,
    か_because_default,
    け__default2,
    け__default3,
    け__default,
    き__default,
    こ__default,
    こ__default2,
    こ__default3,
    く__default,
    く_1_default,
    く__default2,
    だ_default,
    で__default,
    じ__default,
    で_default,
    に_default,
    へ_default,
    と_default,
    を_default,
    は_default,
    よ_default,
    ね_default,
    ___default,
    し__default,
    て__default,
    つ__default,
    う_Verbs_default,
    う_verb_neg_past_default,
    う_verb_past_default,
    うverb__default,
    verb_b_default
  ]
};

/* packages/grammar/src/rules/bunpro/jlpt5/verb-て-b.test.ts */
var negatives = [
  "もっと待って。",
  "少し待って。",
  "こっち来て。",
  "早く来て。",
  "何をしてるの？",
  "彼は今寝ています。",
  "雨が降っている。",
  "壁に絵が掛けてある。",
  "ドアが開いている。",
  "本を読んで。"
];
describe2("bunpro.jlpt5", () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(verb_b_default, "JLPT5", BUNPRO_JLPT5.id, engine.get, { negatives });
});

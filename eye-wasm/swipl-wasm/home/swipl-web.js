var SWIPL = (() => {
    var _scriptDir = typeof document !== 'undefined' && document.currentScript ? document.currentScript.src : undefined;
    if (typeof __filename !== 'undefined') _scriptDir = _scriptDir || __filename;
    return (
        function(SWIPL) {
            SWIPL = SWIPL || {};

            var Module = typeof SWIPL !== "undefined" ? SWIPL : {};
            var readyPromiseResolve, readyPromiseReject;
            Module["ready"] = new Promise(function(resolve, reject) {
                readyPromiseResolve = resolve;
                readyPromiseReject = reject
            });
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_foreign_control")) {
                Object.defineProperty(Module["ready"], "_PL_foreign_control", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_foreign_control on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_foreign_control", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_foreign_control on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_foreign_context")) {
                Object.defineProperty(Module["ready"], "_PL_foreign_context", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_foreign_context on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_foreign_context", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_foreign_context on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_foreign_context_address")) {
                Object.defineProperty(Module["ready"], "_PL_foreign_context_address", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_foreign_context_address on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_foreign_context_address", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_foreign_context_address on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_foreign_context_predicate")) {
                Object.defineProperty(Module["ready"], "_PL_foreign_context_predicate", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_foreign_context_predicate on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_foreign_context_predicate", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_foreign_context_predicate on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_new_atom")) {
                Object.defineProperty(Module["ready"], "_PL_new_atom", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_new_atom on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_new_atom", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_new_atom on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_new_atom_mbchars")) {
                Object.defineProperty(Module["ready"], "_PL_new_atom_mbchars", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_new_atom_mbchars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_new_atom_mbchars", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_new_atom_mbchars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_atom_chars")) {
                Object.defineProperty(Module["ready"], "_PL_atom_chars", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_atom_chars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_atom_chars", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_atom_chars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_new_functor")) {
                Object.defineProperty(Module["ready"], "_PL_new_functor", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_new_functor on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_new_functor", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_new_functor on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_functor_name")) {
                Object.defineProperty(Module["ready"], "_PL_functor_name", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_functor_name on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_functor_name", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_functor_name on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_functor_arity")) {
                Object.defineProperty(Module["ready"], "_PL_functor_arity", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_functor_arity on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_functor_arity", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_functor_arity on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_register_atom")) {
                Object.defineProperty(Module["ready"], "_PL_register_atom", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_register_atom on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_register_atom", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_register_atom on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_unregister_atom")) {
                Object.defineProperty(Module["ready"], "_PL_unregister_atom", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_unregister_atom on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_unregister_atom", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_unregister_atom on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_term_type")) {
                Object.defineProperty(Module["ready"], "_PL_term_type", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_term_type on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_term_type", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_term_type on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_is_variable")) {
                Object.defineProperty(Module["ready"], "_PL_is_variable", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_is_variable on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_is_variable", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_is_variable on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_is_ground")) {
                Object.defineProperty(Module["ready"], "_PL_is_ground", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_is_ground on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_is_ground", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_is_ground on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_is_atom")) {
                Object.defineProperty(Module["ready"], "_PL_is_atom", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_is_atom on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_is_atom", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_is_atom on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_is_string")) {
                Object.defineProperty(Module["ready"], "_PL_is_string", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_is_string on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_is_string", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_is_string on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_is_integer")) {
                Object.defineProperty(Module["ready"], "_PL_is_integer", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_is_integer on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_is_integer", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_is_integer on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_is_float")) {
                Object.defineProperty(Module["ready"], "_PL_is_float", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_is_float on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_is_float", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_is_float on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_is_callable")) {
                Object.defineProperty(Module["ready"], "_PL_is_callable", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_is_callable on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_is_callable", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_is_callable on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_is_compound")) {
                Object.defineProperty(Module["ready"], "_PL_is_compound", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_is_compound on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_is_compound", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_is_compound on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_is_functor")) {
                Object.defineProperty(Module["ready"], "_PL_is_functor", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_is_functor on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_is_functor", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_is_functor on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_is_list")) {
                Object.defineProperty(Module["ready"], "_PL_is_list", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_is_list on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_is_list", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_is_list on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_is_pair")) {
                Object.defineProperty(Module["ready"], "_PL_is_pair", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_is_pair on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_is_pair", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_is_pair on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_is_atomic")) {
                Object.defineProperty(Module["ready"], "_PL_is_atomic", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_is_atomic on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_is_atomic", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_is_atomic on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_is_number")) {
                Object.defineProperty(Module["ready"], "_PL_is_number", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_is_number on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_is_number", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_is_number on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_is_acyclic")) {
                Object.defineProperty(Module["ready"], "_PL_is_acyclic", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_is_acyclic on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_is_acyclic", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_is_acyclic on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_atom")) {
                Object.defineProperty(Module["ready"], "_PL_get_atom", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_atom on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_atom", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_atom on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_atom_chars")) {
                Object.defineProperty(Module["ready"], "_PL_get_atom_chars", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_atom_chars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_atom_chars", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_atom_chars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_chars")) {
                Object.defineProperty(Module["ready"], "_PL_get_chars", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_chars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_chars", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_chars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_list_chars")) {
                Object.defineProperty(Module["ready"], "_PL_get_list_chars", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_list_chars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_list_chars", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_list_chars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_integer")) {
                Object.defineProperty(Module["ready"], "_PL_get_integer", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_integer on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_integer", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_integer on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_long")) {
                Object.defineProperty(Module["ready"], "_PL_get_long", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_long on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_long", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_long on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_int64")) {
                Object.defineProperty(Module["ready"], "_PL_get_int64", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_int64 on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_int64", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_int64 on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_intptr")) {
                Object.defineProperty(Module["ready"], "_PL_get_intptr", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_intptr on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_intptr", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_intptr on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_bool")) {
                Object.defineProperty(Module["ready"], "_PL_get_bool", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_bool on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_bool", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_bool on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_pointer")) {
                Object.defineProperty(Module["ready"], "_PL_get_pointer", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_pointer on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_pointer", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_pointer on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_float")) {
                Object.defineProperty(Module["ready"], "_PL_get_float", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_float on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_float", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_float on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_functor")) {
                Object.defineProperty(Module["ready"], "_PL_get_functor", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_functor on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_functor", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_functor on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_name_arity")) {
                Object.defineProperty(Module["ready"], "_PL_get_name_arity", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_name_arity on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_name_arity", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_name_arity on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_compound_name_arity")) {
                Object.defineProperty(Module["ready"], "_PL_get_compound_name_arity", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_compound_name_arity on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_compound_name_arity", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_compound_name_arity on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_module")) {
                Object.defineProperty(Module["ready"], "_PL_get_module", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_module on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_module", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_module on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_arg")) {
                Object.defineProperty(Module["ready"], "_PL_get_arg", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_arg on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_arg", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_arg on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_atom_nchars")) {
                Object.defineProperty(Module["ready"], "_PL_get_atom_nchars", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_atom_nchars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_atom_nchars", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_atom_nchars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_list_nchars")) {
                Object.defineProperty(Module["ready"], "_PL_get_list_nchars", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_list_nchars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_list_nchars", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_list_nchars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_nchars")) {
                Object.defineProperty(Module["ready"], "_PL_get_nchars", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_nchars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_nchars", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_nchars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_put_atom_nchars")) {
                Object.defineProperty(Module["ready"], "_PL_put_atom_nchars", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_put_atom_nchars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_put_atom_nchars", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_put_atom_nchars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_put_string_nchars")) {
                Object.defineProperty(Module["ready"], "_PL_put_string_nchars", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_put_string_nchars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_put_string_nchars", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_put_string_nchars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_put_list_ncodes")) {
                Object.defineProperty(Module["ready"], "_PL_put_list_ncodes", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_put_list_ncodes on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_put_list_ncodes", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_put_list_ncodes on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_put_list_nchars")) {
                Object.defineProperty(Module["ready"], "_PL_put_list_nchars", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_put_list_nchars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_put_list_nchars", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_put_list_nchars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_unify_atom_nchars")) {
                Object.defineProperty(Module["ready"], "_PL_unify_atom_nchars", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_unify_atom_nchars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_unify_atom_nchars", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_unify_atom_nchars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_unify_string_nchars")) {
                Object.defineProperty(Module["ready"], "_PL_unify_string_nchars", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_unify_string_nchars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_unify_string_nchars", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_unify_string_nchars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_unify_list_ncodes")) {
                Object.defineProperty(Module["ready"], "_PL_unify_list_ncodes", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_unify_list_ncodes on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_unify_list_ncodes", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_unify_list_ncodes on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_unify_list_nchars")) {
                Object.defineProperty(Module["ready"], "_PL_unify_list_nchars", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_unify_list_nchars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_unify_list_nchars", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_unify_list_nchars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_new_atom_nchars")) {
                Object.defineProperty(Module["ready"], "_PL_new_atom_nchars", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_new_atom_nchars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_new_atom_nchars", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_new_atom_nchars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_atom_nchars")) {
                Object.defineProperty(Module["ready"], "_PL_atom_nchars", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_atom_nchars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_atom_nchars", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_atom_nchars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_new_atom_wchars")) {
                Object.defineProperty(Module["ready"], "_PL_new_atom_wchars", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_new_atom_wchars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_new_atom_wchars", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_new_atom_wchars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_atom_wchars")) {
                Object.defineProperty(Module["ready"], "_PL_atom_wchars", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_atom_wchars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_atom_wchars", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_atom_wchars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_wchars")) {
                Object.defineProperty(Module["ready"], "_PL_get_wchars", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_wchars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_wchars", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_wchars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_unify_wchars")) {
                Object.defineProperty(Module["ready"], "_PL_unify_wchars", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_unify_wchars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_unify_wchars", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_unify_wchars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_unify_wchars_diff")) {
                Object.defineProperty(Module["ready"], "_PL_unify_wchars_diff", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_unify_wchars_diff on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_unify_wchars_diff", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_unify_wchars_diff on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_list")) {
                Object.defineProperty(Module["ready"], "_PL_get_list", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_list on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_list", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_list on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_head")) {
                Object.defineProperty(Module["ready"], "_PL_get_head", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_head on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_head", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_head on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_tail")) {
                Object.defineProperty(Module["ready"], "_PL_get_tail", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_tail on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_tail", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_tail on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_nil")) {
                Object.defineProperty(Module["ready"], "_PL_get_nil", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_nil on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_nil", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_nil on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_skip_list")) {
                Object.defineProperty(Module["ready"], "_PL_skip_list", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_skip_list on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_skip_list", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_skip_list on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_put_variable")) {
                Object.defineProperty(Module["ready"], "_PL_put_variable", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_put_variable on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_put_variable", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_put_variable on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_put_atom")) {
                Object.defineProperty(Module["ready"], "_PL_put_atom", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_put_atom on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_put_atom", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_put_atom on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_put_bool")) {
                Object.defineProperty(Module["ready"], "_PL_put_bool", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_put_bool on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_put_bool", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_put_bool on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_put_chars")) {
                Object.defineProperty(Module["ready"], "_PL_put_chars", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_put_chars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_put_chars", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_put_chars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_put_atom_chars")) {
                Object.defineProperty(Module["ready"], "_PL_put_atom_chars", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_put_atom_chars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_put_atom_chars", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_put_atom_chars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_put_string_chars")) {
                Object.defineProperty(Module["ready"], "_PL_put_string_chars", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_put_string_chars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_put_string_chars", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_put_string_chars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_put_list_chars")) {
                Object.defineProperty(Module["ready"], "_PL_put_list_chars", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_put_list_chars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_put_list_chars", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_put_list_chars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_put_integer")) {
                Object.defineProperty(Module["ready"], "_PL_put_integer", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_put_integer on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_put_integer", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_put_integer on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_put_int64")) {
                Object.defineProperty(Module["ready"], "_PL_put_int64", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_put_int64 on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_put_int64", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_put_int64 on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_put_pointer")) {
                Object.defineProperty(Module["ready"], "_PL_put_pointer", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_put_pointer on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_put_pointer", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_put_pointer on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_put_float")) {
                Object.defineProperty(Module["ready"], "_PL_put_float", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_put_float on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_put_float", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_put_float on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_put_functor")) {
                Object.defineProperty(Module["ready"], "_PL_put_functor", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_put_functor on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_put_functor", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_put_functor on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_put_list")) {
                Object.defineProperty(Module["ready"], "_PL_put_list", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_put_list on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_put_list", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_put_list on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_put_nil")) {
                Object.defineProperty(Module["ready"], "_PL_put_nil", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_put_nil on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_put_nil", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_put_nil on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_put_term")) {
                Object.defineProperty(Module["ready"], "_PL_put_term", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_put_term on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_put_term", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_put_term on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_cons_functor")) {
                Object.defineProperty(Module["ready"], "_PL_cons_functor", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_cons_functor on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_cons_functor", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_cons_functor on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_cons_functor_v")) {
                Object.defineProperty(Module["ready"], "_PL_cons_functor_v", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_cons_functor_v on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_cons_functor_v", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_cons_functor_v on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_cons_list")) {
                Object.defineProperty(Module["ready"], "_PL_cons_list", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_cons_list on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_cons_list", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_cons_list on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_unify")) {
                Object.defineProperty(Module["ready"], "_PL_unify", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_unify on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_unify", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_unify on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_unify_atom")) {
                Object.defineProperty(Module["ready"], "_PL_unify_atom", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_unify_atom on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_unify_atom", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_unify_atom on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_unify_bool")) {
                Object.defineProperty(Module["ready"], "_PL_unify_bool", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_unify_bool on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_unify_bool", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_unify_bool on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_unify_chars")) {
                Object.defineProperty(Module["ready"], "_PL_unify_chars", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_unify_chars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_unify_chars", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_unify_chars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_unify_atom_chars")) {
                Object.defineProperty(Module["ready"], "_PL_unify_atom_chars", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_unify_atom_chars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_unify_atom_chars", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_unify_atom_chars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_unify_list_chars")) {
                Object.defineProperty(Module["ready"], "_PL_unify_list_chars", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_unify_list_chars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_unify_list_chars", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_unify_list_chars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_unify_string_chars")) {
                Object.defineProperty(Module["ready"], "_PL_unify_string_chars", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_unify_string_chars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_unify_string_chars", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_unify_string_chars on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_unify_integer")) {
                Object.defineProperty(Module["ready"], "_PL_unify_integer", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_unify_integer on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_unify_integer", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_unify_integer on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_unify_int64")) {
                Object.defineProperty(Module["ready"], "_PL_unify_int64", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_unify_int64 on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_unify_int64", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_unify_int64 on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_unify_uint64")) {
                Object.defineProperty(Module["ready"], "_PL_unify_uint64", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_unify_uint64 on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_unify_uint64", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_unify_uint64 on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_unify_float")) {
                Object.defineProperty(Module["ready"], "_PL_unify_float", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_unify_float on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_unify_float", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_unify_float on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_unify_pointer")) {
                Object.defineProperty(Module["ready"], "_PL_unify_pointer", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_unify_pointer on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_unify_pointer", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_unify_pointer on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_unify_functor")) {
                Object.defineProperty(Module["ready"], "_PL_unify_functor", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_unify_functor on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_unify_functor", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_unify_functor on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_unify_compound")) {
                Object.defineProperty(Module["ready"], "_PL_unify_compound", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_unify_compound on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_unify_compound", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_unify_compound on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_unify_list")) {
                Object.defineProperty(Module["ready"], "_PL_unify_list", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_unify_list on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_unify_list", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_unify_list on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_unify_nil")) {
                Object.defineProperty(Module["ready"], "_PL_unify_nil", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_unify_nil on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_unify_nil", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_unify_nil on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_unify_arg")) {
                Object.defineProperty(Module["ready"], "_PL_unify_arg", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_unify_arg on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_unify_arg", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_unify_arg on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_unify_term")) {
                Object.defineProperty(Module["ready"], "_PL_unify_term", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_unify_term on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_unify_term", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_unify_term on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_chars_to_term")) {
                Object.defineProperty(Module["ready"], "_PL_chars_to_term", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_chars_to_term on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_chars_to_term", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_chars_to_term on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_wchars_to_term")) {
                Object.defineProperty(Module["ready"], "_PL_wchars_to_term", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_wchars_to_term on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_wchars_to_term", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_wchars_to_term on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_quote")) {
                Object.defineProperty(Module["ready"], "_PL_quote", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_quote on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_quote", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_quote on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_atom_ex")) {
                Object.defineProperty(Module["ready"], "_PL_get_atom_ex", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_atom_ex on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_atom_ex", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_atom_ex on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_integer_ex")) {
                Object.defineProperty(Module["ready"], "_PL_get_integer_ex", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_integer_ex on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_integer_ex", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_integer_ex on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_long_ex")) {
                Object.defineProperty(Module["ready"], "_PL_get_long_ex", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_long_ex on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_long_ex", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_long_ex on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_int64_ex")) {
                Object.defineProperty(Module["ready"], "_PL_get_int64_ex", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_int64_ex on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_int64_ex", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_int64_ex on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_intptr_ex")) {
                Object.defineProperty(Module["ready"], "_PL_get_intptr_ex", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_intptr_ex on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_intptr_ex", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_intptr_ex on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_size_ex")) {
                Object.defineProperty(Module["ready"], "_PL_get_size_ex", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_size_ex on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_size_ex", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_size_ex on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_bool_ex")) {
                Object.defineProperty(Module["ready"], "_PL_get_bool_ex", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_bool_ex on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_bool_ex", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_bool_ex on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_float_ex")) {
                Object.defineProperty(Module["ready"], "_PL_get_float_ex", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_float_ex on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_float_ex", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_float_ex on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_char_ex")) {
                Object.defineProperty(Module["ready"], "_PL_get_char_ex", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_char_ex on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_char_ex", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_char_ex on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_pointer_ex")) {
                Object.defineProperty(Module["ready"], "_PL_get_pointer_ex", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_pointer_ex on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_pointer_ex", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_pointer_ex on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_list_ex")) {
                Object.defineProperty(Module["ready"], "_PL_get_list_ex", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_list_ex on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_list_ex", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_list_ex on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_nil_ex")) {
                Object.defineProperty(Module["ready"], "_PL_get_nil_ex", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_nil_ex on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_nil_ex", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_nil_ex on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_unify_list_ex")) {
                Object.defineProperty(Module["ready"], "_PL_unify_list_ex", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_unify_list_ex on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_unify_list_ex", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_unify_list_ex on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_unify_nil_ex")) {
                Object.defineProperty(Module["ready"], "_PL_unify_nil_ex", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_unify_nil_ex on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_unify_nil_ex", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_unify_nil_ex on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_unify_bool_ex")) {
                Object.defineProperty(Module["ready"], "_PL_unify_bool_ex", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_unify_bool_ex on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_unify_bool_ex", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_unify_bool_ex on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_instantiation_error")) {
                Object.defineProperty(Module["ready"], "_PL_instantiation_error", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_instantiation_error on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_instantiation_error", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_instantiation_error on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_uninstantiation_error")) {
                Object.defineProperty(Module["ready"], "_PL_uninstantiation_error", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_uninstantiation_error on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_uninstantiation_error", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_uninstantiation_error on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_representation_error")) {
                Object.defineProperty(Module["ready"], "_PL_representation_error", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_representation_error on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_representation_error", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_representation_error on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_type_error")) {
                Object.defineProperty(Module["ready"], "_PL_type_error", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_type_error on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_type_error", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_type_error on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_domain_error")) {
                Object.defineProperty(Module["ready"], "_PL_domain_error", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_domain_error on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_domain_error", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_domain_error on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_existence_error")) {
                Object.defineProperty(Module["ready"], "_PL_existence_error", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_existence_error on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_existence_error", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_existence_error on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_permission_error")) {
                Object.defineProperty(Module["ready"], "_PL_permission_error", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_permission_error on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_permission_error", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_permission_error on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_resource_error")) {
                Object.defineProperty(Module["ready"], "_PL_resource_error", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_resource_error on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_resource_error", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_resource_error on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_syntax_error")) {
                Object.defineProperty(Module["ready"], "_PL_syntax_error", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_syntax_error on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_syntax_error", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_syntax_error on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_unregister_blob_type")) {
                Object.defineProperty(Module["ready"], "_PL_unregister_blob_type", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_unregister_blob_type on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_unregister_blob_type", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_unregister_blob_type on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_is_blob")) {
                Object.defineProperty(Module["ready"], "_PL_is_blob", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_is_blob on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_is_blob", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_is_blob on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_unify_blob")) {
                Object.defineProperty(Module["ready"], "_PL_unify_blob", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_unify_blob on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_unify_blob", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_unify_blob on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_put_blob")) {
                Object.defineProperty(Module["ready"], "_PL_put_blob", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_put_blob on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_put_blob", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_put_blob on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_blob")) {
                Object.defineProperty(Module["ready"], "_PL_get_blob", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_blob on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_blob", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_blob on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_blob_data")) {
                Object.defineProperty(Module["ready"], "_PL_blob_data", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_blob_data on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_blob_data", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_blob_data on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_pred")) {
                Object.defineProperty(Module["ready"], "_PL_pred", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_pred on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_pred", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_pred on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_predicate")) {
                Object.defineProperty(Module["ready"], "_PL_predicate", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_predicate on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_predicate", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_predicate on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_predicate_info")) {
                Object.defineProperty(Module["ready"], "_PL_predicate_info", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_predicate_info on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_predicate_info", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_predicate_info on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_open_query")) {
                Object.defineProperty(Module["ready"], "_PL_open_query", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_open_query on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_open_query", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_open_query on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_next_solution")) {
                Object.defineProperty(Module["ready"], "_PL_next_solution", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_next_solution on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_next_solution", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_next_solution on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_cut_query")) {
                Object.defineProperty(Module["ready"], "_PL_cut_query", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_cut_query on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_cut_query", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_cut_query on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_close_query")) {
                Object.defineProperty(Module["ready"], "_PL_close_query", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_close_query on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_close_query", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_close_query on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_current_query")) {
                Object.defineProperty(Module["ready"], "_PL_current_query", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_current_query on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_current_query", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_current_query on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_call_predicate")) {
                Object.defineProperty(Module["ready"], "_PL_call_predicate", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_call_predicate on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_call_predicate", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_call_predicate on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_call")) {
                Object.defineProperty(Module["ready"], "_PL_call", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_call on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_call", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_call on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_open_foreign_frame")) {
                Object.defineProperty(Module["ready"], "_PL_open_foreign_frame", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_open_foreign_frame on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_open_foreign_frame", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_open_foreign_frame on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_close_foreign_frame")) {
                Object.defineProperty(Module["ready"], "_PL_close_foreign_frame", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_close_foreign_frame on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_close_foreign_frame", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_close_foreign_frame on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_discard_foreign_frame")) {
                Object.defineProperty(Module["ready"], "_PL_discard_foreign_frame", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_discard_foreign_frame on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_discard_foreign_frame", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_discard_foreign_frame on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_rewind_foreign_frame")) {
                Object.defineProperty(Module["ready"], "_PL_rewind_foreign_frame", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_rewind_foreign_frame on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_rewind_foreign_frame", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_rewind_foreign_frame on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_context")) {
                Object.defineProperty(Module["ready"], "_PL_context", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_context on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_context", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_context on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_strip_module")) {
                Object.defineProperty(Module["ready"], "_PL_strip_module", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_strip_module on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_strip_module", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_strip_module on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_module_name")) {
                Object.defineProperty(Module["ready"], "_PL_module_name", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_module_name on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_module_name", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_module_name on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_new_module")) {
                Object.defineProperty(Module["ready"], "_PL_new_module", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_new_module on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_new_module", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_new_module on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_raise_exception")) {
                Object.defineProperty(Module["ready"], "_PL_raise_exception", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_raise_exception on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_raise_exception", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_raise_exception on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_throw")) {
                Object.defineProperty(Module["ready"], "_PL_throw", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_throw on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_throw", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_throw on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_exception")) {
                Object.defineProperty(Module["ready"], "_PL_exception", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_exception on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_exception", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_exception on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_clear_exception")) {
                Object.defineProperty(Module["ready"], "_PL_clear_exception", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_clear_exception on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_clear_exception", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_clear_exception on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_sigaction")) {
                Object.defineProperty(Module["ready"], "_PL_sigaction", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_sigaction on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_sigaction", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_sigaction on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_signal")) {
                Object.defineProperty(Module["ready"], "_PL_signal", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_signal on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_signal", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_signal on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_raise")) {
                Object.defineProperty(Module["ready"], "_PL_raise", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_raise on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_raise", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_raise on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_handle_signals")) {
                Object.defineProperty(Module["ready"], "_PL_handle_signals", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_handle_signals on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_handle_signals", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_handle_signals on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_signum_ex")) {
                Object.defineProperty(Module["ready"], "_PL_get_signum_ex", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_signum_ex on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_signum_ex", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_signum_ex on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_compare")) {
                Object.defineProperty(Module["ready"], "_PL_compare", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_compare on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_compare", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_compare on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_same_compound")) {
                Object.defineProperty(Module["ready"], "_PL_same_compound", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_same_compound on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_same_compound", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_same_compound on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_record")) {
                Object.defineProperty(Module["ready"], "_PL_record", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_record on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_record", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_record on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_duplicate_record")) {
                Object.defineProperty(Module["ready"], "_PL_duplicate_record", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_duplicate_record on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_duplicate_record", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_duplicate_record on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_recorded")) {
                Object.defineProperty(Module["ready"], "_PL_recorded", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_recorded on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_recorded", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_recorded on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_erase")) {
                Object.defineProperty(Module["ready"], "_PL_erase", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_erase on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_erase", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_erase on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_record_external")) {
                Object.defineProperty(Module["ready"], "_PL_record_external", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_record_external on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_record_external", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_record_external on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_recorded_external")) {
                Object.defineProperty(Module["ready"], "_PL_recorded_external", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_recorded_external on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_recorded_external", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_recorded_external on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_erase_external")) {
                Object.defineProperty(Module["ready"], "_PL_erase_external", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_erase_external on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_erase_external", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_erase_external on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_file_name")) {
                Object.defineProperty(Module["ready"], "_PL_get_file_name", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_file_name on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_file_name", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_file_name on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_get_file_nameW")) {
                Object.defineProperty(Module["ready"], "_PL_get_file_nameW", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_get_file_nameW on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_get_file_nameW", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_get_file_nameW on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_set_prolog_flag")) {
                Object.defineProperty(Module["ready"], "_PL_set_prolog_flag", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_set_prolog_flag on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_set_prolog_flag", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_set_prolog_flag on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_warning")) {
                Object.defineProperty(Module["ready"], "_PL_warning", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_warning on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_warning", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_warning on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_action")) {
                Object.defineProperty(Module["ready"], "_PL_action", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_action on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_action", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_action on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_query")) {
                Object.defineProperty(Module["ready"], "_PL_query", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_query on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_query", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_query on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_register_foreign_in_module")) {
                Object.defineProperty(Module["ready"], "_PL_register_foreign_in_module", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_register_foreign_in_module on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_register_foreign_in_module", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_register_foreign_in_module on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_register_foreign")) {
                Object.defineProperty(Module["ready"], "_PL_register_foreign", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_register_foreign on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_register_foreign", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_register_foreign on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_register_extensions_in_module")) {
                Object.defineProperty(Module["ready"], "_PL_register_extensions_in_module", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_register_extensions_in_module on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_register_extensions_in_module", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_register_extensions_in_module on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_register_extensions")) {
                Object.defineProperty(Module["ready"], "_PL_register_extensions", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_register_extensions on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_register_extensions", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_register_extensions on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_dispatch_hook")) {
                Object.defineProperty(Module["ready"], "_PL_dispatch_hook", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_dispatch_hook on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_dispatch_hook", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_dispatch_hook on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_abort_hook")) {
                Object.defineProperty(Module["ready"], "_PL_abort_hook", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_abort_hook on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_abort_hook", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_abort_hook on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_abort_unhook")) {
                Object.defineProperty(Module["ready"], "_PL_abort_unhook", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_abort_unhook on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_abort_unhook", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_abort_unhook on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_on_halt")) {
                Object.defineProperty(Module["ready"], "_PL_on_halt", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_on_halt on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_on_halt", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_on_halt on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_exit_hook")) {
                Object.defineProperty(Module["ready"], "_PL_exit_hook", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_exit_hook on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_exit_hook", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_exit_hook on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_agc_hook")) {
                Object.defineProperty(Module["ready"], "_PL_agc_hook", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_agc_hook on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_agc_hook", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_agc_hook on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_initialise")) {
                Object.defineProperty(Module["ready"], "_PL_initialise", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_initialise on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_initialise", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_initialise on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_is_initialised")) {
                Object.defineProperty(Module["ready"], "_PL_is_initialised", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_is_initialised on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_is_initialised", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_is_initialised on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_set_resource_db_mem")) {
                Object.defineProperty(Module["ready"], "_PL_set_resource_db_mem", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_set_resource_db_mem on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_set_resource_db_mem", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_set_resource_db_mem on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_toplevel")) {
                Object.defineProperty(Module["ready"], "_PL_toplevel", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_toplevel on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_toplevel", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_toplevel on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_cleanup")) {
                Object.defineProperty(Module["ready"], "_PL_cleanup", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_cleanup on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_cleanup", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_cleanup on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_cleanup_fork")) {
                Object.defineProperty(Module["ready"], "_PL_cleanup_fork", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_cleanup_fork on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_cleanup_fork", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_cleanup_fork on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_halt")) {
                Object.defineProperty(Module["ready"], "_PL_halt", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_halt on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_halt", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_halt on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_new_term_ref")) {
                Object.defineProperty(Module["ready"], "_PL_new_term_ref", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_new_term_ref on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_new_term_ref", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_new_term_ref on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_new_term_refs")) {
                Object.defineProperty(Module["ready"], "_PL_new_term_refs", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_new_term_refs on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_new_term_refs", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_new_term_refs on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_copy_term_ref")) {
                Object.defineProperty(Module["ready"], "_PL_copy_term_ref", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_copy_term_ref on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_copy_term_ref", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_copy_term_ref on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "_PL_reset_term_refs")) {
                Object.defineProperty(Module["ready"], "_PL_reset_term_refs", {
                    configurable: true,
                    get: function() {
                        abort("You are getting _PL_reset_term_refs on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "_PL_reset_term_refs", {
                    configurable: true,
                    set: function() {
                        abort("You are setting _PL_reset_term_refs on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "___stdio_exit")) {
                Object.defineProperty(Module["ready"], "___stdio_exit", {
                    configurable: true,
                    get: function() {
                        abort("You are getting ___stdio_exit on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "___stdio_exit", {
                    configurable: true,
                    set: function() {
                        abort("You are setting ___stdio_exit on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module["ready"], "onRuntimeInitialized")) {
                Object.defineProperty(Module["ready"], "onRuntimeInitialized", {
                    configurable: true,
                    get: function() {
                        abort("You are getting onRuntimeInitialized on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                });
                Object.defineProperty(Module["ready"], "onRuntimeInitialized", {
                    configurable: true,
                    set: function() {
                        abort("You are setting onRuntimeInitialized on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")
                    }
                })
            }
            if (!Module.expectedDataFileDownloads) {
                Module.expectedDataFileDownloads = 0
            }
            Module.expectedDataFileDownloads++;
            (function() {
                if (Module["ENVIRONMENT_IS_PTHREAD"]) return;
                var loadPackage = function(metadata) {
                    var PACKAGE_PATH = "";
                    if (typeof window === "object") {
                        PACKAGE_PATH = window["encodeURIComponent"](window.location.pathname.toString().substring(0, window.location.pathname.toString().lastIndexOf("/")) + "/")
                    } else if (typeof process === "undefined" && typeof location !== "undefined") {
                        PACKAGE_PATH = encodeURIComponent(location.pathname.toString().substring(0, location.pathname.toString().lastIndexOf("/")) + "/")
                    }
                    var PACKAGE_NAME = "src/swipl-web.data";
                    var REMOTE_PACKAGE_BASE = "swipl-web.data";
                    if (typeof Module["locateFilePackage"] === "function" && !Module["locateFile"]) {
                        Module["locateFile"] = Module["locateFilePackage"];
                        err("warning: you defined Module.locateFilePackage, that has been renamed to Module.locateFile (using your locateFilePackage for now)")
                    }
                    var REMOTE_PACKAGE_NAME = Module["locateFile"] ? Module["locateFile"](REMOTE_PACKAGE_BASE, "") : REMOTE_PACKAGE_BASE;
                    var REMOTE_PACKAGE_SIZE = metadata["remote_package_size"];
                    var PACKAGE_UUID = metadata["package_uuid"];

                    function fetchRemotePackage(packageName, packageSize, callback, errback) {
                        if (typeof process === "object" && typeof process.versions === "object" && typeof process.versions.node === "string") {
                            require("fs").readFile(packageName, function(err, contents) {
                                if (err) {
                                    errback(err)
                                } else {
                                    callback(contents.buffer)
                                }
                            });
                            return
                        }
                        var xhr = new XMLHttpRequest;
                        xhr.open("GET", packageName, true);
                        xhr.responseType = "arraybuffer";
                        xhr.onprogress = function(event) {
                            var url = packageName;
                            var size = packageSize;
                            if (event.total) size = event.total;
                            if (event.loaded) {
                                if (!xhr.addedTotal) {
                                    xhr.addedTotal = true;
                                    if (!Module.dataFileDownloads) Module.dataFileDownloads = {};
                                    Module.dataFileDownloads[url] = {
                                        loaded: event.loaded,
                                        total: size
                                    }
                                } else {
                                    Module.dataFileDownloads[url].loaded = event.loaded
                                }
                                var total = 0;
                                var loaded = 0;
                                var num = 0;
                                for (var download in Module.dataFileDownloads) {
                                    var data = Module.dataFileDownloads[download];
                                    total += data.total;
                                    loaded += data.loaded;
                                    num++
                                }
                                total = Math.ceil(total * Module.expectedDataFileDownloads / num);
                                if (Module["setStatus"]) Module["setStatus"]("Downloading data... (" + loaded + "/" + total + ")")
                            } else if (!Module.dataFileDownloads) {
                                if (Module["setStatus"]) Module["setStatus"]("Downloading data...")
                            }
                        };
                        xhr.onerror = function(event) {
                            throw new Error("NetworkError for: " + packageName)
                        };
                        xhr.onload = function(event) {
                            if (xhr.status == 200 || xhr.status == 304 || xhr.status == 206 || xhr.status == 0 && xhr.response) {
                                var packageData = xhr.response;
                                callback(packageData)
                            } else {
                                throw new Error(xhr.statusText + " : " + xhr.responseURL)
                            }
                        };
                        xhr.send(null)
                    }

                    function handleError(error) {
                        console.error("package error:", error)
                    }
                    var fetchedCallback = null;
                    var fetched = Module["getPreloadedPackage"] ? Module["getPreloadedPackage"](REMOTE_PACKAGE_NAME, REMOTE_PACKAGE_SIZE) : null;
                    if (!fetched) fetchRemotePackage(REMOTE_PACKAGE_NAME, REMOTE_PACKAGE_SIZE, function(data) {
                        if (fetchedCallback) {
                            fetchedCallback(data);
                            fetchedCallback = null
                        } else {
                            fetched = data
                        }
                    }, handleError);

                    function runWithFS() {
                        function assert(check, msg) {
                            if (!check) throw msg + (new Error).stack
                        }
                        Module["FS_createPath"]("/", "src", true, true);
                        Module["FS_createPath"]("/src", "wasm-preload", true, true);
                        Module["FS_createPath"]("/src/wasm-preload", "library", true, true);
                        Module["FS_createPath"]("/src/wasm-preload/library", "iri_scheme", true, true);
                        Module["FS_createPath"]("/src/wasm-preload/library", "theme", true, true);
                        Module["FS_createPath"]("/src/wasm-preload/library", "lynx", true, true);
                        Module["FS_createPath"]("/src/wasm-preload/library", "clp", true, true);
                        Module["FS_createPath"]("/src/wasm-preload/library", "dcg", true, true);
                        Module["FS_createPath"]("/src/wasm-preload/library", "unicode", true, true);
                        Module["FS_createPath"]("/src/wasm-preload/library", "build", true, true);
                        Module["FS_createPath"]("/src/wasm-preload/library", "dialect", true, true);
                        Module["FS_createPath"]("/src/wasm-preload/library/dialect", "swi", true, true);
                        Module["FS_createPath"]("/src/wasm-preload/library/dialect", "xsb", true, true);
                        Module["FS_createPath"]("/src/wasm-preload/library/dialect", "sicstus4", true, true);
                        Module["FS_createPath"]("/src/wasm-preload/library/dialect", "eclipse", true, true);
                        Module["FS_createPath"]("/src/wasm-preload/library/dialect", "yap", true, true);
                        Module["FS_createPath"]("/src/wasm-preload/library/dialect", "sicstus", true, true);
                        Module["FS_createPath"]("/src/wasm-preload/library/dialect", "hprolog", true, true);
                        Module["FS_createPath"]("/src/wasm-preload/library/dialect", "iso", true, true);

                        function DataRequest(start, end, audio) {
                            this.start = start;
                            this.end = end;
                            this.audio = audio
                        }
                        DataRequest.prototype = {
                            requests: {},
                            open: function(mode, name) {
                                this.name = name;
                                this.requests[name] = this;
                                Module["addRunDependency"]("fp " + this.name)
                            },
                            send: function() {},
                            onload: function() {
                                var byteArray = this.byteArray.subarray(this.start, this.end);
                                this.finish(byteArray)
                            },
                            finish: function(byteArray) {
                                var that = this;
                                Module["FS_createDataFile"](this.name, null, byteArray, true, true, true);
                                Module["removeRunDependency"]("fp " + that.name);
                                this.requests[this.name] = null
                            }
                        };
                        var files = metadata["files"];
                        for (var i = 0; i < files.length; ++i) {
                            new DataRequest(files[i]["start"], files[i]["end"], files[i]["audio"] || 0).open("GET", files[i]["filename"])
                        }

                        function processPackageData(arrayBuffer) {
                            assert(arrayBuffer, "Loading data file failed.");
                            assert(arrayBuffer instanceof ArrayBuffer, "bad input to processPackageData");
                            var byteArray = new Uint8Array(arrayBuffer);
                            DataRequest.prototype.byteArray = byteArray;
                            var files = metadata["files"];
                            for (var i = 0; i < files.length; ++i) {
                                DataRequest.prototype.requests[files[i].filename].onload()
                            }
                            Module["removeRunDependency"]("datafile_src/swipl-web.data")
                        }
                        Module["addRunDependency"]("datafile_src/swipl-web.data");
                        if (!Module.preloadResults) Module.preloadResults = {};
                        Module.preloadResults[PACKAGE_NAME] = {
                            fromCache: false
                        };
                        if (fetched) {
                            processPackageData(fetched);
                            fetched = null
                        } else {
                            fetchedCallback = processPackageData
                        }
                    }
                    if (Module["calledRun"]) {
                        runWithFS()
                    } else {
                        if (!Module["preRun"]) Module["preRun"] = [];
                        Module["preRun"].push(runWithFS)
                    }
                };
                loadPackage({
                    "files": [{
                        "filename": "/src/wasm-preload/boot.prc",
                        "start": 0,
                        "end": 105882
                    }, {
                        "filename": "/src/wasm-preload/library/predicate_options.pl",
                        "start": 105882,
                        "end": 136924
                    }, {
                        "filename": "/src/wasm-preload/library/coinduction.pl",
                        "start": 136924,
                        "end": 142984
                    }, {
                        "filename": "/src/wasm-preload/library/sort.pl",
                        "start": 142984,
                        "end": 146804
                    }, {
                        "filename": "/src/wasm-preload/library/readutil.pl",
                        "start": 146804,
                        "end": 157893
                    }, {
                        "filename": "/src/wasm-preload/library/portray_text.pl",
                        "start": 157893,
                        "end": 166635
                    }, {
                        "filename": "/src/wasm-preload/library/prolog_source.pl",
                        "start": 166635,
                        "end": 203399
                    }, {
                        "filename": "/src/wasm-preload/library/settings.pl",
                        "start": 203399,
                        "end": 227363
                    }, {
                        "filename": "/src/wasm-preload/library/obfuscate.pl",
                        "start": 227363,
                        "end": 231485
                    }, {
                        "filename": "/src/wasm-preload/library/prolog_breakpoints.pl",
                        "start": 231485,
                        "end": 242326
                    }, {
                        "filename": "/src/wasm-preload/library/main.pl",
                        "start": 242326,
                        "end": 272890
                    }, {
                        "filename": "/src/wasm-preload/library/fastrw.pl",
                        "start": 272890,
                        "end": 277279
                    }, {
                        "filename": "/src/wasm-preload/library/varnumbers.pl",
                        "start": 277279,
                        "end": 284420
                    }, {
                        "filename": "/src/wasm-preload/library/ctypes.pl",
                        "start": 284420,
                        "end": 289442
                    }, {
                        "filename": "/src/wasm-preload/library/prolog_codewalk.pl",
                        "start": 289442,
                        "end": 328904
                    }, {
                        "filename": "/src/wasm-preload/library/prolog_autoload.pl",
                        "start": 328904,
                        "end": 337453
                    }, {
                        "filename": "/src/wasm-preload/library/quintus.pl",
                        "start": 337453,
                        "end": 350546
                    }, {
                        "filename": "/src/wasm-preload/library/utf8.pl",
                        "start": 350546,
                        "end": 355151
                    }, {
                        "filename": "/src/wasm-preload/library/check.pl",
                        "start": 355151,
                        "end": 388648
                    }, {
                        "filename": "/src/wasm-preload/library/ordsets.pl",
                        "start": 388648,
                        "end": 405141
                    }, {
                        "filename": "/src/wasm-preload/library/writef.pl",
                        "start": 405141,
                        "end": 415062
                    }, {
                        "filename": "/src/wasm-preload/library/apply.pl",
                        "start": 415062,
                        "end": 428858
                    }, {
                        "filename": "/src/wasm-preload/library/heaps.pl",
                        "start": 428858,
                        "end": 437135
                    }, {
                        "filename": "/src/wasm-preload/library/ugraphs.pl",
                        "start": 437135,
                        "end": 457599
                    }, {
                        "filename": "/src/wasm-preload/library/tty.pl",
                        "start": 457599,
                        "end": 466874
                    }, {
                        "filename": "/src/wasm-preload/library/files.pl",
                        "start": 466874,
                        "end": 469631
                    }, {
                        "filename": "/src/wasm-preload/library/optparse.pl",
                        "start": 469631,
                        "end": 507398
                    }, {
                        "filename": "/src/wasm-preload/library/atom.pl",
                        "start": 507398,
                        "end": 512820
                    }, {
                        "filename": "/src/wasm-preload/library/prolog_deps.pl",
                        "start": 512820,
                        "end": 530003
                    }, {
                        "filename": "/src/wasm-preload/library/gensym.pl",
                        "start": 530003,
                        "end": 533465
                    }, {
                        "filename": "/src/wasm-preload/library/prolog_install.pl",
                        "start": 533465,
                        "end": 539247
                    }, {
                        "filename": "/src/wasm-preload/library/yall.pl",
                        "start": 539247,
                        "end": 558757
                    }, {
                        "filename": "/src/wasm-preload/library/qsave.pl",
                        "start": 558757,
                        "end": 600651
                    }, {
                        "filename": "/src/wasm-preload/library/occurs.pl",
                        "start": 600651,
                        "end": 607482
                    }, {
                        "filename": "/src/wasm-preload/library/aggregate.pl",
                        "start": 607482,
                        "end": 631855
                    }, {
                        "filename": "/src/wasm-preload/library/hotfix.pl",
                        "start": 631855,
                        "end": 639737
                    }, {
                        "filename": "/src/wasm-preload/library/git.pl",
                        "start": 639737,
                        "end": 667736
                    }, {
                        "filename": "/src/wasm-preload/library/debug.pl",
                        "start": 667736,
                        "end": 681140
                    }, {
                        "filename": "/src/wasm-preload/library/nb_set.pl",
                        "start": 681140,
                        "end": 686806
                    }, {
                        "filename": "/src/wasm-preload/library/dicts.pl",
                        "start": 686806,
                        "end": 697462
                    }, {
                        "filename": "/src/wasm-preload/library/zip.pl",
                        "start": 697462,
                        "end": 705103
                    }, {
                        "filename": "/src/wasm-preload/library/prolog_clause.pl",
                        "start": 705103,
                        "end": 738992
                    }, {
                        "filename": "/src/wasm-preload/library/persistency.pl",
                        "start": 738992,
                        "end": 760911
                    }, {
                        "filename": "/src/wasm-preload/library/vm.pl",
                        "start": 760911,
                        "end": 769036
                    }, {
                        "filename": "/src/wasm-preload/library/prolog_pack.pl",
                        "start": 769036,
                        "end": 845908
                    }, {
                        "filename": "/src/wasm-preload/library/shlib.pl",
                        "start": 845908,
                        "end": 865770
                    }, {
                        "filename": "/src/wasm-preload/library/prolog_code.pl",
                        "start": 865770,
                        "end": 878031
                    }, {
                        "filename": "/src/wasm-preload/library/explain.pl",
                        "start": 878031,
                        "end": 892226
                    }, {
                        "filename": "/src/wasm-preload/library/solution_sequences.pl",
                        "start": 892226,
                        "end": 904478
                    }, {
                        "filename": "/src/wasm-preload/library/checkselect.pl",
                        "start": 904478,
                        "end": 907673
                    }, {
                        "filename": "/src/wasm-preload/library/operators.pl",
                        "start": 907673,
                        "end": 912917
                    }, {
                        "filename": "/src/wasm-preload/library/nb_rbtrees.pl",
                        "start": 912917,
                        "end": 920855
                    }, {
                        "filename": "/src/wasm-preload/library/prolog_xref.qlf",
                        "start": 920855,
                        "end": 956968
                    }, {
                        "filename": "/src/wasm-preload/library/pprint.pl",
                        "start": 956968,
                        "end": 985192
                    }, {
                        "filename": "/src/wasm-preload/library/arithmetic.pl",
                        "start": 985192,
                        "end": 994278
                    }, {
                        "filename": "/src/wasm-preload/library/www_browser.pl",
                        "start": 994278,
                        "end": 1002578
                    }, {
                        "filename": "/src/wasm-preload/library/ansi_term.pl",
                        "start": 1002578,
                        "end": 1021271
                    }, {
                        "filename": "/src/wasm-preload/library/edit.pl",
                        "start": 1021271,
                        "end": 1040384
                    }, {
                        "filename": "/src/wasm-preload/library/iostream.pl",
                        "start": 1040384,
                        "end": 1049136
                    }, {
                        "filename": "/src/wasm-preload/library/shell.pl",
                        "start": 1049136,
                        "end": 1059841
                    }, {
                        "filename": "/src/wasm-preload/library/when.pl",
                        "start": 1059841,
                        "end": 1067598
                    }, {
                        "filename": "/src/wasm-preload/library/pio.pl",
                        "start": 1067598,
                        "end": 1069525
                    }, {
                        "filename": "/src/wasm-preload/library/quasi_quotations.pl",
                        "start": 1069525,
                        "end": 1080911
                    }, {
                        "filename": "/src/wasm-preload/library/prolog_xref.pl",
                        "start": 1080911,
                        "end": 1173108
                    }, {
                        "filename": "/src/wasm-preload/library/check_installation.pl",
                        "start": 1173108,
                        "end": 1198206
                    }, {
                        "filename": "/src/wasm-preload/library/base64.pl",
                        "start": 1198206,
                        "end": 1210659
                    }, {
                        "filename": "/src/wasm-preload/library/error.pl",
                        "start": 1210659,
                        "end": 1226535
                    }, {
                        "filename": "/src/wasm-preload/library/date.pl",
                        "start": 1226535,
                        "end": 1235987
                    }, {
                        "filename": "/src/wasm-preload/library/prolog_colour.qlf",
                        "start": 1235987,
                        "end": 1281128
                    }, {
                        "filename": "/src/wasm-preload/library/prolog_config.pl",
                        "start": 1281128,
                        "end": 1285936
                    }, {
                        "filename": "/src/wasm-preload/library/increval.pl",
                        "start": 1285936,
                        "end": 1293214
                    }, {
                        "filename": "/src/wasm-preload/library/codesio.pl",
                        "start": 1293214,
                        "end": 1299663
                    }, {
                        "filename": "/src/wasm-preload/library/rbtrees.pl",
                        "start": 1299663,
                        "end": 1337060
                    }, {
                        "filename": "/src/wasm-preload/library/prolog_versions.pl",
                        "start": 1337060,
                        "end": 1343985
                    }, {
                        "filename": "/src/wasm-preload/library/wfs.pl",
                        "start": 1343985,
                        "end": 1350906
                    }, {
                        "filename": "/src/wasm-preload/library/listing.pl",
                        "start": 1350906,
                        "end": 1388703
                    }, {
                        "filename": "/src/wasm-preload/library/broadcast.pl",
                        "start": 1388703,
                        "end": 1394058
                    }, {
                        "filename": "/src/wasm-preload/library/random.pl",
                        "start": 1394058,
                        "end": 1407507
                    }, {
                        "filename": "/src/wasm-preload/library/sandbox.pl",
                        "start": 1407507,
                        "end": 1449932
                    }, {
                        "filename": "/src/wasm-preload/library/make.pl",
                        "start": 1449932,
                        "end": 1456552
                    }, {
                        "filename": "/src/wasm-preload/library/oset.pl",
                        "start": 1456552,
                        "end": 1461245
                    }, {
                        "filename": "/src/wasm-preload/library/modules.pl",
                        "start": 1461245,
                        "end": 1466094
                    }, {
                        "filename": "/src/wasm-preload/library/intercept.pl",
                        "start": 1466094,
                        "end": 1474716
                    }, {
                        "filename": "/src/wasm-preload/library/prolog_colour.pl",
                        "start": 1474716,
                        "end": 1577504
                    }, {
                        "filename": "/src/wasm-preload/library/strings.pl",
                        "start": 1577504,
                        "end": 1593052
                    }, {
                        "filename": "/src/wasm-preload/library/hashtable.pl",
                        "start": 1593052,
                        "end": 1603376
                    }, {
                        "filename": "/src/wasm-preload/library/url.pl",
                        "start": 1603376,
                        "end": 1631529
                    }, {
                        "filename": "/src/wasm-preload/library/record.pl",
                        "start": 1631529,
                        "end": 1648137
                    }, {
                        "filename": "/src/wasm-preload/library/prolog_format.pl",
                        "start": 1648137,
                        "end": 1655e3
                    }, {
                        "filename": "/src/wasm-preload/library/qpforeign.pl",
                        "start": 1655e3,
                        "end": 1677305
                    }, {
                        "filename": "/src/wasm-preload/library/tabling.pl",
                        "start": 1677305,
                        "end": 1679105
                    }, {
                        "filename": "/src/wasm-preload/library/prolog_debug.pl",
                        "start": 1679105,
                        "end": 1687712
                    }, {
                        "filename": "/src/wasm-preload/library/dif.pl",
                        "start": 1687712,
                        "end": 1700318
                    }, {
                        "filename": "/src/wasm-preload/library/threadutil.pl",
                        "start": 1700318,
                        "end": 1715621
                    }, {
                        "filename": "/src/wasm-preload/library/assoc.pl",
                        "start": 1715621,
                        "end": 1733948
                    }, {
                        "filename": "/src/wasm-preload/library/prolog_wrap.pl",
                        "start": 1733948,
                        "end": 1739352
                    }, {
                        "filename": "/src/wasm-preload/library/prolog_trace.pl",
                        "start": 1739352,
                        "end": 1747011
                    }, {
                        "filename": "/src/wasm-preload/library/charsio.pl",
                        "start": 1747011,
                        "end": 1753647
                    }, {
                        "filename": "/src/wasm-preload/library/pure_input.pl",
                        "start": 1753647,
                        "end": 1763519
                    }, {
                        "filename": "/src/wasm-preload/library/base32.pl",
                        "start": 1763519,
                        "end": 1771812
                    }, {
                        "filename": "/src/wasm-preload/library/apply_macros.pl",
                        "start": 1771812,
                        "end": 1786695
                    }, {
                        "filename": "/src/wasm-preload/library/prolog_jiti.pl",
                        "start": 1786695,
                        "end": 1791880
                    }, {
                        "filename": "/src/wasm-preload/library/system.pl",
                        "start": 1791880,
                        "end": 1795188
                    }, {
                        "filename": "/src/wasm-preload/library/thread.pl",
                        "start": 1795188,
                        "end": 1822718
                    }, {
                        "filename": "/src/wasm-preload/library/csv.pl",
                        "start": 1822718,
                        "end": 1841539
                    }, {
                        "filename": "/src/wasm-preload/library/prolog_metainference.pl",
                        "start": 1841539,
                        "end": 1851353
                    }, {
                        "filename": "/src/wasm-preload/library/dialect.pl",
                        "start": 1851353,
                        "end": 1855446
                    }, {
                        "filename": "/src/wasm-preload/library/backcomp.pl",
                        "start": 1855446,
                        "end": 1875343
                    }, {
                        "filename": "/src/wasm-preload/library/terms.pl",
                        "start": 1875343,
                        "end": 1890036
                    }, {
                        "filename": "/src/wasm-preload/library/edinburgh.pl",
                        "start": 1890036,
                        "end": 1894541
                    }, {
                        "filename": "/src/wasm-preload/library/INDEX.pl",
                        "start": 1894541,
                        "end": 1939026
                    }, {
                        "filename": "/src/wasm-preload/library/readln.pl",
                        "start": 1939026,
                        "end": 1947953
                    }, {
                        "filename": "/src/wasm-preload/library/prolog_stack.pl",
                        "start": 1947953,
                        "end": 1973728
                    }, {
                        "filename": "/src/wasm-preload/library/tables.pl",
                        "start": 1973728,
                        "end": 1986339
                    }, {
                        "filename": "/src/wasm-preload/library/statistics.pl",
                        "start": 1986339,
                        "end": 2010909
                    }, {
                        "filename": "/src/wasm-preload/library/pairs.pl",
                        "start": 2010909,
                        "end": 2016773
                    }, {
                        "filename": "/src/wasm-preload/library/.created",
                        "start": 2016773,
                        "end": 2016773
                    }, {
                        "filename": "/src/wasm-preload/library/prolog_history.pl",
                        "start": 2016773,
                        "end": 2022638
                    }, {
                        "filename": "/src/wasm-preload/library/lazy_lists.pl",
                        "start": 2022638,
                        "end": 2039138
                    }, {
                        "filename": "/src/wasm-preload/library/checklast.pl",
                        "start": 2039138,
                        "end": 2042394
                    }, {
                        "filename": "/src/wasm-preload/library/lists.pl",
                        "start": 2042394,
                        "end": 2066635
                    }, {
                        "filename": "/src/wasm-preload/library/console_input.pl",
                        "start": 2066635,
                        "end": 2070322
                    }, {
                        "filename": "/src/wasm-preload/library/thread_pool.pl",
                        "start": 2070322,
                        "end": 2087203
                    }, {
                        "filename": "/src/wasm-preload/library/option.pl",
                        "start": 2087203,
                        "end": 2099817
                    }, {
                        "filename": "/src/wasm-preload/library/iri_scheme/file.pl",
                        "start": 2099817,
                        "end": 2102642
                    }, {
                        "filename": "/src/wasm-preload/library/iri_scheme/.created",
                        "start": 2102642,
                        "end": 2102642
                    }, {
                        "filename": "/src/wasm-preload/library/theme/auto.pl",
                        "start": 2102642,
                        "end": 2104867
                    }, {
                        "filename": "/src/wasm-preload/library/theme/dark.pl",
                        "start": 2104867,
                        "end": 2116771
                    }, {
                        "filename": "/src/wasm-preload/library/theme/.created",
                        "start": 2116771,
                        "end": 2116771
                    }, {
                        "filename": "/src/wasm-preload/library/lynx/html_text.pl",
                        "start": 2116771,
                        "end": 2141509
                    }, {
                        "filename": "/src/wasm-preload/library/lynx/format.pl",
                        "start": 2141509,
                        "end": 2152300
                    }, {
                        "filename": "/src/wasm-preload/library/lynx/html_style.pl",
                        "start": 2152300,
                        "end": 2156781
                    }, {
                        "filename": "/src/wasm-preload/library/lynx/pldoc_style.pl",
                        "start": 2156781,
                        "end": 2160026
                    }, {
                        "filename": "/src/wasm-preload/library/lynx/INDEX.pl",
                        "start": 2160026,
                        "end": 2160550
                    }, {
                        "filename": "/src/wasm-preload/library/lynx/.created",
                        "start": 2160550,
                        "end": 2160550
                    }, {
                        "filename": "/src/wasm-preload/library/clp/clp_distinct.pl",
                        "start": 2160550,
                        "end": 2167148
                    }, {
                        "filename": "/src/wasm-preload/library/clp/bounds.pl",
                        "start": 2167148,
                        "end": 2206457
                    }, {
                        "filename": "/src/wasm-preload/library/clp/clpb.pl",
                        "start": 2206457,
                        "end": 2272509
                    }, {
                        "filename": "/src/wasm-preload/library/clp/clp_events.pl",
                        "start": 2272509,
                        "end": 2275221
                    }, {
                        "filename": "/src/wasm-preload/library/clp/INDEX.pl",
                        "start": 2275221,
                        "end": 2276586
                    }, {
                        "filename": "/src/wasm-preload/library/clp/.created",
                        "start": 2276586,
                        "end": 2276586
                    }, {
                        "filename": "/src/wasm-preload/library/clp/clpfd.pl",
                        "start": 2276586,
                        "end": 2553251
                    }, {
                        "filename": "/src/wasm-preload/library/dcg/high_order.pl",
                        "start": 2553251,
                        "end": 2560727
                    }, {
                        "filename": "/src/wasm-preload/library/dcg/basics.pl",
                        "start": 2560727,
                        "end": 2571608
                    }, {
                        "filename": "/src/wasm-preload/library/dcg/INDEX.pl",
                        "start": 2571608,
                        "end": 2572922
                    }, {
                        "filename": "/src/wasm-preload/library/dcg/.created",
                        "start": 2572922,
                        "end": 2572922
                    }, {
                        "filename": "/src/wasm-preload/library/unicode/blocks.pl",
                        "start": 2572922,
                        "end": 2583163
                    }, {
                        "filename": "/src/wasm-preload/library/unicode/INDEX.pl",
                        "start": 2583163,
                        "end": 2583337
                    }, {
                        "filename": "/src/wasm-preload/library/unicode/.created",
                        "start": 2583337,
                        "end": 2583337
                    }, {
                        "filename": "/src/wasm-preload/library/unicode/unicode_data.pl",
                        "start": 2583337,
                        "end": 2588786
                    }, {
                        "filename": "/src/wasm-preload/library/build/conan.pl",
                        "start": 2588786,
                        "end": 2595781
                    }, {
                        "filename": "/src/wasm-preload/library/build/tools.pl",
                        "start": 2595781,
                        "end": 2622055
                    }, {
                        "filename": "/src/wasm-preload/library/build/cmake.pl",
                        "start": 2622055,
                        "end": 2626912
                    }, {
                        "filename": "/src/wasm-preload/library/build/make.pl",
                        "start": 2626912,
                        "end": 2632914
                    }, {
                        "filename": "/src/wasm-preload/library/build/.created",
                        "start": 2632914,
                        "end": 2632914
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/yap.pl",
                        "start": 2632914,
                        "end": 2640048
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/bim.pl",
                        "start": 2640048,
                        "end": 2644329
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/sicstus4.pl",
                        "start": 2644329,
                        "end": 2651716
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/xsb.pl",
                        "start": 2651716,
                        "end": 2672959
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/hprolog.pl",
                        "start": 2672959,
                        "end": 2681351
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/ifprolog.pl",
                        "start": 2681351,
                        "end": 2717808
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/commons.pl",
                        "start": 2717808,
                        "end": 2720368
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/sicstus.pl",
                        "start": 2720368,
                        "end": 2737190
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/.created",
                        "start": 2737190,
                        "end": 2737190
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/swi/syspred_options.pl",
                        "start": 2737190,
                        "end": 2744950
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/swi/.created",
                        "start": 2744950,
                        "end": 2744950
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/xsb/source.pl",
                        "start": 2744950,
                        "end": 2754746
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/xsb/curr_sym.pl",
                        "start": 2754746,
                        "end": 2757009
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/xsb/storage.pl",
                        "start": 2757009,
                        "end": 2759579
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/xsb/consult.pl",
                        "start": 2759579,
                        "end": 2761421
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/xsb/ordsets.pl",
                        "start": 2761421,
                        "end": 2763643
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/xsb/README.md",
                        "start": 2763643,
                        "end": 2764294
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/xsb/gensym.pl",
                        "start": 2764294,
                        "end": 2766336
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/xsb/gpp.pl",
                        "start": 2766336,
                        "end": 2771400
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/xsb/machine.pl",
                        "start": 2771400,
                        "end": 2778442
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/xsb/timed_call.pl",
                        "start": 2778442,
                        "end": 2784181
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/xsb/string.pl",
                        "start": 2784181,
                        "end": 2786735
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/xsb/standard.pl",
                        "start": 2786735,
                        "end": 2791540
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/xsb/basics.pl",
                        "start": 2791540,
                        "end": 2798907
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/xsb/intern.pl",
                        "start": 2798907,
                        "end": 2800829
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/xsb/thread.pl",
                        "start": 2800829,
                        "end": 2803014
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/xsb/.created",
                        "start": 2803014,
                        "end": 2803014
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/xsb/error_handler.pl",
                        "start": 2803014,
                        "end": 2807211
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/xsb/lists.pl",
                        "start": 2807211,
                        "end": 2809109
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/xsb/setof.pl",
                        "start": 2809109,
                        "end": 2811648
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/sicstus4/ordsets.pl",
                        "start": 2811648,
                        "end": 2813670
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/sicstus4/timeout.pl",
                        "start": 2813670,
                        "end": 2815450
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/sicstus4/file_systems.pl",
                        "start": 2815450,
                        "end": 2832933
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/sicstus4/sets.pl",
                        "start": 2832933,
                        "end": 2836479
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/sicstus4/aggregate.pl",
                        "start": 2836479,
                        "end": 2838899
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/sicstus4/types.pl",
                        "start": 2838899,
                        "end": 2843192
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/sicstus4/between.pl",
                        "start": 2843192,
                        "end": 2845321
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/sicstus4/system.pl",
                        "start": 2845321,
                        "end": 2848234
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/sicstus4/samsort.pl",
                        "start": 2848234,
                        "end": 2851196
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/sicstus4/terms.pl",
                        "start": 2851196,
                        "end": 2854088
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/sicstus4/.created",
                        "start": 2854088,
                        "end": 2854088
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/sicstus4/lists.pl",
                        "start": 2854088,
                        "end": 2862080
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/sicstus4/clpfd.pl",
                        "start": 2862080,
                        "end": 2864620
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/sicstus4/sockets.pl",
                        "start": 2864620,
                        "end": 2870817
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/eclipse/test_util_iso.pl",
                        "start": 2870817,
                        "end": 2880669
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/eclipse/.created",
                        "start": 2880669,
                        "end": 2880669
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/yap/README.TXT",
                        "start": 2880669,
                        "end": 2881020
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/yap/.created",
                        "start": 2881020,
                        "end": 2881020
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/sicstus/ordsets.pl",
                        "start": 2881020,
                        "end": 2882974
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/sicstus/timeout.pl",
                        "start": 2882974,
                        "end": 2886725
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/sicstus/README.TXT",
                        "start": 2886725,
                        "end": 2886756
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/sicstus/swipl-lfr.pl",
                        "start": 2886756,
                        "end": 2890756
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/sicstus/block.pl",
                        "start": 2890756,
                        "end": 2901199
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/sicstus/arrays.pl",
                        "start": 2901199,
                        "end": 2904903
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/sicstus/system.pl",
                        "start": 2904903,
                        "end": 2911478
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/sicstus/terms.pl",
                        "start": 2911478,
                        "end": 2913717
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/sicstus/.created",
                        "start": 2913717,
                        "end": 2913717
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/sicstus/lists.pl",
                        "start": 2913717,
                        "end": 2918374
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/sicstus/sockets.pl",
                        "start": 2918374,
                        "end": 2924593
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/hprolog/format.pl",
                        "start": 2924593,
                        "end": 2926430
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/hprolog/.created",
                        "start": 2926430,
                        "end": 2926430
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/iso/iso_predicates.pl",
                        "start": 2926430,
                        "end": 2936120
                    }, {
                        "filename": "/src/wasm-preload/library/dialect/iso/.created",
                        "start": 2936120,
                        "end": 2936120
                    }],
                    "remote_package_size": 2936120,
                    "package_uuid": "2446fad0-698d-4294-be7a-4f265b1cebbb"
                })
            })();
            if (Module["ENVIRONMENT_IS_PTHREAD"]) Module["preRun"] = [];
            var necessaryPreJSTasks = Module["preRun"].slice();
            if (!Module["preRun"]) throw "Module.preRun should exist because file support used it; did a pre-js delete it?";
            necessaryPreJSTasks.forEach(function(task) {
                if (Module["preRun"].indexOf(task) < 0) throw "All preRun tasks that exist before user pre-js code should remain after; did you replace Module or modify Module.preRun?"
            });
            var moduleOverrides = Object.assign({}, Module);
            var arguments_ = [];
            var thisProgram = "./this.program";
            var quit_ = (status, toThrow) => {
                throw toThrow
            };
            var ENVIRONMENT_IS_WEB = typeof window === "object";
            var ENVIRONMENT_IS_WORKER = typeof importScripts === "function";
            var ENVIRONMENT_IS_NODE = typeof process === "object" && typeof process.versions === "object" && typeof process.versions.node === "string";
            var ENVIRONMENT_IS_SHELL = !ENVIRONMENT_IS_WEB && !ENVIRONMENT_IS_NODE && !ENVIRONMENT_IS_WORKER;
            if (Module["ENVIRONMENT"]) {
                throw new Error("Module.ENVIRONMENT has been deprecated. To force the environment, use the ENVIRONMENT compile-time option (for example, -s ENVIRONMENT=web or -s ENVIRONMENT=node)")
            }
            var scriptDirectory = "";

            function locateFile(path) {
                if (Module["locateFile"]) {
                    return Module["locateFile"](path, scriptDirectory)
                }
                return scriptDirectory + path
            }
            var read_, readAsync, readBinary, setWindowTitle;

            function logExceptionOnExit(e) {
                if (e instanceof ExitStatus) return;
                let toLog = e;
                if (e && typeof e === "object" && e.stack) {
                    toLog = [e, e.stack]
                }
                err("exiting due to exception: " + toLog)
            }
            var fs;
            var nodePath;
            var requireNodeFS;
            if (ENVIRONMENT_IS_NODE) {
                if (!(typeof process === "object" && typeof require === "function")) throw new Error("not compiled for this environment (did you build to HTML and try to run it not on the web, or set ENVIRONMENT to something - like node - and run it someplace else - like on the web?)");
                if (ENVIRONMENT_IS_WORKER) {
                    scriptDirectory = require("path").dirname(scriptDirectory) + "/"
                } else {
                    scriptDirectory = __dirname + "/"
                }
                requireNodeFS = (() => {
                    if (!nodePath) {
                        fs = require("fs");
                        nodePath = require("path")
                    }
                });
                read_ = function shell_read(filename, binary) {
                    requireNodeFS();
                    filename = nodePath["normalize"](filename);
                    return fs.readFileSync(filename, binary ? undefined : "utf8")
                };
                readBinary = (filename => {
                    var ret = read_(filename, true);
                    if (!ret.buffer) {
                        ret = new Uint8Array(ret)
                    }
                    assert(ret.buffer);
                    return ret
                });
                readAsync = ((filename, onload, onerror) => {
                    requireNodeFS();
                    filename = nodePath["normalize"](filename);
                    fs.readFile(filename, function(err, data) {
                        if (err) onerror(err);
                        else onload(data.buffer)
                    })
                });
                if (process["argv"].length > 1) {
                    thisProgram = process["argv"][1].replace(/\\/g, "/")
                }
                arguments_ = process["argv"].slice(2);
                process["on"]("uncaughtException", function(ex) {
                    if (!(ex instanceof ExitStatus)) {
                        throw ex
                    }
                });
                process["on"]("unhandledRejection", function(reason) {
                    throw reason
                });
                quit_ = ((status, toThrow) => {
                    if (keepRuntimeAlive()) {
                        process["exitCode"] = status;
                        throw toThrow
                    }
                    logExceptionOnExit(toThrow);
                    process["exit"](status)
                });
                Module["inspect"] = function() {
                    return "[Emscripten Module object]"
                }
            } else if (ENVIRONMENT_IS_SHELL) {
                if (typeof process === "object" && typeof require === "function" || typeof window === "object" || typeof importScripts === "function") throw new Error("not compiled for this environment (did you build to HTML and try to run it not on the web, or set ENVIRONMENT to something - like node - and run it someplace else - like on the web?)");
                if (typeof read != "undefined") {
                    read_ = function shell_read(f) {
                        return read(f)
                    }
                }
                readBinary = function readBinary(f) {
                    let data;
                    if (typeof readbuffer === "function") {
                        return new Uint8Array(readbuffer(f))
                    }
                    data = read(f, "binary");
                    assert(typeof data === "object");
                    return data
                };
                readAsync = function readAsync(f, onload, onerror) {
                    setTimeout(() => onload(readBinary(f)), 0)
                };
                if (typeof scriptArgs != "undefined") {
                    arguments_ = scriptArgs
                } else if (typeof arguments != "undefined") {
                    arguments_ = arguments
                }
                if (typeof quit === "function") {
                    quit_ = ((status, toThrow) => {
                        logExceptionOnExit(toThrow);
                        quit(status)
                    })
                }
                if (typeof print !== "undefined") {
                    if (typeof console === "undefined") console = {};
                    console.log = print;
                    console.warn = console.error = typeof printErr !== "undefined" ? printErr : print
                }
            } else if (ENVIRONMENT_IS_WEB || ENVIRONMENT_IS_WORKER) {
                if (ENVIRONMENT_IS_WORKER) {
                    scriptDirectory = self.location.href
                } else if (typeof document !== "undefined" && document.currentScript) {
                    scriptDirectory = document.currentScript.src
                }
                if (_scriptDir) {
                    scriptDirectory = _scriptDir
                }
                if (scriptDirectory.indexOf("blob:") !== 0) {
                    scriptDirectory = scriptDirectory.substr(0, scriptDirectory.replace(/[?#].*/, "").lastIndexOf("/") + 1)
                } else {
                    scriptDirectory = ""
                }
                if (!(typeof window === "object" || typeof importScripts === "function")) throw new Error("not compiled for this environment (did you build to HTML and try to run it not on the web, or set ENVIRONMENT to something - like node - and run it someplace else - like on the web?)"); {
                    read_ = (url => {
                        var xhr = new XMLHttpRequest;
                        xhr.open("GET", url, false);
                        xhr.send(null);
                        return xhr.responseText
                    });
                    if (ENVIRONMENT_IS_WORKER) {
                        readBinary = (url => {
                            var xhr = new XMLHttpRequest;
                            xhr.open("GET", url, false);
                            xhr.responseType = "arraybuffer";
                            xhr.send(null);
                            return new Uint8Array(xhr.response)
                        })
                    }
                    readAsync = ((url, onload, onerror) => {
                        var xhr = new XMLHttpRequest;
                        xhr.open("GET", url, true);
                        xhr.responseType = "arraybuffer";
                        xhr.onload = (() => {
                            if (xhr.status == 200 || xhr.status == 0 && xhr.response) {
                                onload(xhr.response);
                                return
                            }
                            onerror()
                        });
                        xhr.onerror = onerror;
                        xhr.send(null)
                    })
                }
                setWindowTitle = (title => document.title = title)
            } else {
                throw new Error("environment detection error")
            }
            var out = Module["print"] || console.log.bind(console);
            var err = Module["printErr"] || console.warn.bind(console);
            Object.assign(Module, moduleOverrides);
            moduleOverrides = null;
            if (Module["arguments"]) arguments_ = Module["arguments"];
            if (!Object.getOwnPropertyDescriptor(Module, "arguments")) {
                Object.defineProperty(Module, "arguments", {
                    configurable: true,
                    get: function() {
                        abort("Module.arguments has been replaced with plain arguments_ (the initial value can be provided on Module, but after startup the value is only looked for on a local variable of that name)")
                    }
                })
            }
            if (Module["thisProgram"]) thisProgram = Module["thisProgram"];
            if (!Object.getOwnPropertyDescriptor(Module, "thisProgram")) {
                Object.defineProperty(Module, "thisProgram", {
                    configurable: true,
                    get: function() {
                        abort("Module.thisProgram has been replaced with plain thisProgram (the initial value can be provided on Module, but after startup the value is only looked for on a local variable of that name)")
                    }
                })
            }
            if (Module["quit"]) quit_ = Module["quit"];
            if (!Object.getOwnPropertyDescriptor(Module, "quit")) {
                Object.defineProperty(Module, "quit", {
                    configurable: true,
                    get: function() {
                        abort("Module.quit has been replaced with plain quit_ (the initial value can be provided on Module, but after startup the value is only looked for on a local variable of that name)")
                    }
                })
            }
            assert(typeof Module["memoryInitializerPrefixURL"] === "undefined", "Module.memoryInitializerPrefixURL option was removed, use Module.locateFile instead");
            assert(typeof Module["pthreadMainPrefixURL"] === "undefined", "Module.pthreadMainPrefixURL option was removed, use Module.locateFile instead");
            assert(typeof Module["cdInitializerPrefixURL"] === "undefined", "Module.cdInitializerPrefixURL option was removed, use Module.locateFile instead");
            assert(typeof Module["filePackagePrefixURL"] === "undefined", "Module.filePackagePrefixURL option was removed, use Module.locateFile instead");
            assert(typeof Module["read"] === "undefined", "Module.read option was removed (modify read_ in JS)");
            assert(typeof Module["readAsync"] === "undefined", "Module.readAsync option was removed (modify readAsync in JS)");
            assert(typeof Module["readBinary"] === "undefined", "Module.readBinary option was removed (modify readBinary in JS)");
            assert(typeof Module["setWindowTitle"] === "undefined", "Module.setWindowTitle option was removed (modify setWindowTitle in JS)");
            assert(typeof Module["TOTAL_MEMORY"] === "undefined", "Module.TOTAL_MEMORY has been renamed Module.INITIAL_MEMORY");
            if (!Object.getOwnPropertyDescriptor(Module, "read")) {
                Object.defineProperty(Module, "read", {
                    configurable: true,
                    get: function() {
                        abort("Module.read has been replaced with plain read_ (the initial value can be provided on Module, but after startup the value is only looked for on a local variable of that name)")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module, "readAsync")) {
                Object.defineProperty(Module, "readAsync", {
                    configurable: true,
                    get: function() {
                        abort("Module.readAsync has been replaced with plain readAsync (the initial value can be provided on Module, but after startup the value is only looked for on a local variable of that name)")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module, "readBinary")) {
                Object.defineProperty(Module, "readBinary", {
                    configurable: true,
                    get: function() {
                        abort("Module.readBinary has been replaced with plain readBinary (the initial value can be provided on Module, but after startup the value is only looked for on a local variable of that name)")
                    }
                })
            }
            if (!Object.getOwnPropertyDescriptor(Module, "setWindowTitle")) {
                Object.defineProperty(Module, "setWindowTitle", {
                    configurable: true,
                    get: function() {
                        abort("Module.setWindowTitle has been replaced with plain setWindowTitle (the initial value can be provided on Module, but after startup the value is only looked for on a local variable of that name)")
                    }
                })
            }
            assert(!ENVIRONMENT_IS_SHELL, "shell environment detected but not enabled at build time.  Add 'shell' to `-s ENVIRONMENT` to enable.");
            var POINTER_SIZE = 4;

            function warnOnce(text) {
                if (!warnOnce.shown) warnOnce.shown = {};
                if (!warnOnce.shown[text]) {
                    warnOnce.shown[text] = 1;
                    err(text)
                }
            }

            function convertJsFunctionToWasm(func, sig) {
                if (typeof WebAssembly.Function === "function") {
                    var typeNames = {
                        "i": "i32",
                        "j": "i64",
                        "f": "f32",
                        "d": "f64"
                    };
                    var type = {
                        parameters: [],
                        results: sig[0] == "v" ? [] : [typeNames[sig[0]]]
                    };
                    for (var i = 1; i < sig.length; ++i) {
                        type.parameters.push(typeNames[sig[i]])
                    }
                    return new WebAssembly.Function(type, func)
                }
                var typeSection = [1, 0, 1, 96];
                var sigRet = sig.slice(0, 1);
                var sigParam = sig.slice(1);
                var typeCodes = {
                    "i": 127,
                    "j": 126,
                    "f": 125,
                    "d": 124
                };
                typeSection.push(sigParam.length);
                for (var i = 0; i < sigParam.length; ++i) {
                    typeSection.push(typeCodes[sigParam[i]])
                }
                if (sigRet == "v") {
                    typeSection.push(0)
                } else {
                    typeSection = typeSection.concat([1, typeCodes[sigRet]])
                }
                typeSection[1] = typeSection.length - 2;
                var bytes = new Uint8Array([0, 97, 115, 109, 1, 0, 0, 0].concat(typeSection, [2, 7, 1, 1, 101, 1, 102, 0, 0, 7, 5, 1, 1, 102, 0, 0]));
                var module = new WebAssembly.Module(bytes);
                var instance = new WebAssembly.Instance(module, {
                    "e": {
                        "f": func
                    }
                });
                var wrappedFunc = instance.exports["f"];
                return wrappedFunc
            }
            var freeTableIndexes = [];
            var functionsInTableMap;

            function getEmptyTableSlot() {
                if (freeTableIndexes.length) {
                    return freeTableIndexes.pop()
                }
                try {
                    wasmTable.grow(1)
                } catch (err) {
                    if (!(err instanceof RangeError)) {
                        throw err
                    }
                    throw "Unable to grow wasm table. Set ALLOW_TABLE_GROWTH."
                }
                return wasmTable.length - 1
            }

            function updateTableMap(offset, count) {
                for (var i = offset; i < offset + count; i++) {
                    var item = getWasmTableEntry(i);
                    if (item) {
                        functionsInTableMap.set(item, i)
                    }
                }
            }
            var tempRet0 = 0;
            var setTempRet0 = value => {
                tempRet0 = value
            };
            var getTempRet0 = () => tempRet0;
            var wasmBinary;
            if (Module["wasmBinary"]) wasmBinary = Module["wasmBinary"];
            if (!Object.getOwnPropertyDescriptor(Module, "wasmBinary")) {
                Object.defineProperty(Module, "wasmBinary", {
                    configurable: true,
                    get: function() {
                        abort("Module.wasmBinary has been replaced with plain wasmBinary (the initial value can be provided on Module, but after startup the value is only looked for on a local variable of that name)")
                    }
                })
            }
            var noExitRuntime = Module["noExitRuntime"] || false;
            if (!Object.getOwnPropertyDescriptor(Module, "noExitRuntime")) {
                Object.defineProperty(Module, "noExitRuntime", {
                    configurable: true,
                    get: function() {
                        abort("Module.noExitRuntime has been replaced with plain noExitRuntime (the initial value can be provided on Module, but after startup the value is only looked for on a local variable of that name)")
                    }
                })
            }
            if (typeof WebAssembly !== "object") {
                abort("no native wasm support detected")
            }

            function setValue(ptr, value, type = "i8", noSafe) {
                if (type.charAt(type.length - 1) === "*") type = "i32";
                switch (type) {
                    case "i1":
                        HEAP8[ptr >> 0] = value;
                        break;
                    case "i8":
                        HEAP8[ptr >> 0] = value;
                        break;
                    case "i16":
                        HEAP16[ptr >> 1] = value;
                        break;
                    case "i32":
                        HEAP32[ptr >> 2] = value;
                        break;
                    case "i64":
                        tempI64 = [value >>> 0, (tempDouble = value, +Math.abs(tempDouble) >= 1 ? tempDouble > 0 ? (Math.min(+Math.floor(tempDouble / 4294967296), 4294967295) | 0) >>> 0 : ~~+Math.ceil((tempDouble - +(~~tempDouble >>> 0)) / 4294967296) >>> 0 : 0)], HEAP32[ptr >> 2] = tempI64[0], HEAP32[ptr + 4 >> 2] = tempI64[1];
                        break;
                    case "float":
                        HEAPF32[ptr >> 2] = value;
                        break;
                    case "double":
                        HEAPF64[ptr >> 3] = value;
                        break;
                    default:
                        abort("invalid type for setValue: " + type)
                }
            }

            function getValue(ptr, type = "i8", noSafe) {
                if (type.charAt(type.length - 1) === "*") type = "i32";
                switch (type) {
                    case "i1":
                        return HEAP8[ptr >> 0];
                    case "i8":
                        return HEAP8[ptr >> 0];
                    case "i16":
                        return HEAP16[ptr >> 1];
                    case "i32":
                        return HEAP32[ptr >> 2];
                    case "i64":
                        return HEAP32[ptr >> 2];
                    case "float":
                        return HEAPF32[ptr >> 2];
                    case "double":
                        return Number(HEAPF64[ptr >> 3]);
                    default:
                        abort("invalid type for getValue: " + type)
                }
                return null
            }
            var wasmMemory;
            var ABORT = false;
            var EXITSTATUS;

            function assert(condition, text) {
                if (!condition) {
                    abort("Assertion failed" + (text ? ": " + text : ""))
                }
            }

            function getCFunc(ident) {
                var func = Module["_" + ident];
                assert(func, "Cannot call unknown function " + ident + ", make sure it is exported");
                return func
            }

            function ccall(ident, returnType, argTypes, args, opts) {
                var toC = {
                    "string": function(str) {
                        var ret = 0;
                        if (str !== null && str !== undefined && str !== 0) {
                            var len = (str.length << 2) + 1;
                            ret = stackAlloc(len);
                            stringToUTF8(str, ret, len)
                        }
                        return ret
                    },
                    "array": function(arr) {
                        var ret = stackAlloc(arr.length);
                        writeArrayToMemory(arr, ret);
                        return ret
                    }
                };

                function convertReturnValue(ret) {
                    if (returnType === "string") return UTF8ToString(ret);
                    if (returnType === "boolean") return Boolean(ret);
                    return ret
                }
                var func = getCFunc(ident);
                var cArgs = [];
                var stack = 0;
                assert(returnType !== "array", 'Return type should not be "array".');
                if (args) {
                    for (var i = 0; i < args.length; i++) {
                        var converter = toC[argTypes[i]];
                        if (converter) {
                            if (stack === 0) stack = stackSave();
                            cArgs[i] = converter(args[i])
                        } else {
                            cArgs[i] = args[i]
                        }
                    }
                }
                var ret = func.apply(null, cArgs);

                function onDone(ret) {
                    if (stack !== 0) stackRestore(stack);
                    return convertReturnValue(ret)
                }
                ret = onDone(ret);
                return ret
            }

            function cwrap(ident, returnType, argTypes, opts) {
                return function() {
                    return ccall(ident, returnType, argTypes, arguments, opts)
                }
            }
            var ALLOC_NORMAL = 0;
            var ALLOC_STACK = 1;

            function allocate(slab, allocator) {
                var ret;
                assert(typeof allocator === "number", "allocate no longer takes a type argument");
                assert(typeof slab !== "number", "allocate no longer takes a number as arg0");
                if (allocator == ALLOC_STACK) {
                    ret = stackAlloc(slab.length)
                } else {
                    ret = _malloc(slab.length)
                }
                if (!slab.subarray && !slab.slice) {
                    slab = new Uint8Array(slab)
                }
                HEAPU8.set(slab, ret);
                return ret
            }
            var UTF8Decoder = typeof TextDecoder !== "undefined" ? new TextDecoder("utf8") : undefined;

            function UTF8ArrayToString(heap, idx, maxBytesToRead) {
                var endIdx = idx + maxBytesToRead;
                var endPtr = idx;
                while (heap[endPtr] && !(endPtr >= endIdx)) ++endPtr;
                if (endPtr - idx > 16 && heap.subarray && UTF8Decoder) {
                    return UTF8Decoder.decode(heap.subarray(idx, endPtr))
                } else {
                    var str = "";
                    while (idx < endPtr) {
                        var u0 = heap[idx++];
                        if (!(u0 & 128)) {
                            str += String.fromCharCode(u0);
                            continue
                        }
                        var u1 = heap[idx++] & 63;
                        if ((u0 & 224) == 192) {
                            str += String.fromCharCode((u0 & 31) << 6 | u1);
                            continue
                        }
                        var u2 = heap[idx++] & 63;
                        if ((u0 & 240) == 224) {
                            u0 = (u0 & 15) << 12 | u1 << 6 | u2
                        } else {
                            if ((u0 & 248) != 240) warnOnce("Invalid UTF-8 leading byte 0x" + u0.toString(16) + " encountered when deserializing a UTF-8 string in wasm memory to a JS string!");
                            u0 = (u0 & 7) << 18 | u1 << 12 | u2 << 6 | heap[idx++] & 63
                        }
                        if (u0 < 65536) {
                            str += String.fromCharCode(u0)
                        } else {
                            var ch = u0 - 65536;
                            str += String.fromCharCode(55296 | ch >> 10, 56320 | ch & 1023)
                        }
                    }
                }
                return str
            }

            function UTF8ToString(ptr, maxBytesToRead) {
                return ptr ? UTF8ArrayToString(HEAPU8, ptr, maxBytesToRead) : ""
            }

            function stringToUTF8Array(str, heap, outIdx, maxBytesToWrite) {
                if (!(maxBytesToWrite > 0)) return 0;
                var startIdx = outIdx;
                var endIdx = outIdx + maxBytesToWrite - 1;
                for (var i = 0; i < str.length; ++i) {
                    var u = str.charCodeAt(i);
                    if (u >= 55296 && u <= 57343) {
                        var u1 = str.charCodeAt(++i);
                        u = 65536 + ((u & 1023) << 10) | u1 & 1023
                    }
                    if (u <= 127) {
                        if (outIdx >= endIdx) break;
                        heap[outIdx++] = u
                    } else if (u <= 2047) {
                        if (outIdx + 1 >= endIdx) break;
                        heap[outIdx++] = 192 | u >> 6;
                        heap[outIdx++] = 128 | u & 63
                    } else if (u <= 65535) {
                        if (outIdx + 2 >= endIdx) break;
                        heap[outIdx++] = 224 | u >> 12;
                        heap[outIdx++] = 128 | u >> 6 & 63;
                        heap[outIdx++] = 128 | u & 63
                    } else {
                        if (outIdx + 3 >= endIdx) break;
                        if (u > 1114111) warnOnce("Invalid Unicode code point 0x" + u.toString(16) + " encountered when serializing a JS string to a UTF-8 string in wasm memory! (Valid unicode code points should be in range 0-0x10FFFF).");
                        heap[outIdx++] = 240 | u >> 18;
                        heap[outIdx++] = 128 | u >> 12 & 63;
                        heap[outIdx++] = 128 | u >> 6 & 63;
                        heap[outIdx++] = 128 | u & 63
                    }
                }
                heap[outIdx] = 0;
                return outIdx - startIdx
            }

            function stringToUTF8(str, outPtr, maxBytesToWrite) {
                assert(typeof maxBytesToWrite == "number", "stringToUTF8(str, outPtr, maxBytesToWrite) is missing the third parameter that specifies the length of the output buffer!");
                return stringToUTF8Array(str, HEAPU8, outPtr, maxBytesToWrite)
            }

            function lengthBytesUTF8(str) {
                var len = 0;
                for (var i = 0; i < str.length; ++i) {
                    var u = str.charCodeAt(i);
                    if (u >= 55296 && u <= 57343) u = 65536 + ((u & 1023) << 10) | str.charCodeAt(++i) & 1023;
                    if (u <= 127) ++len;
                    else if (u <= 2047) len += 2;
                    else if (u <= 65535) len += 3;
                    else len += 4
                }
                return len
            }
            var UTF16Decoder = typeof TextDecoder !== "undefined" ? new TextDecoder("utf-16le") : undefined;

            function allocateUTF8(str) {
                var size = lengthBytesUTF8(str) + 1;
                var ret = _malloc(size);
                if (ret) stringToUTF8Array(str, HEAP8, ret, size);
                return ret
            }

            function writeArrayToMemory(array, buffer) {
                assert(array.length >= 0, "writeArrayToMemory array must have a length (should be an array or typed array)");
                HEAP8.set(array, buffer)
            }

            function writeAsciiToMemory(str, buffer, dontAddNull) {
                for (var i = 0; i < str.length; ++i) {
                    assert(str.charCodeAt(i) === (str.charCodeAt(i) & 255));
                    HEAP8[buffer++ >> 0] = str.charCodeAt(i)
                }
                if (!dontAddNull) HEAP8[buffer >> 0] = 0
            }

            function alignUp(x, multiple) {
                if (x % multiple > 0) {
                    x += multiple - x % multiple
                }
                return x
            }
            var buffer, HEAP8, HEAPU8, HEAP16, HEAPU16, HEAP32, HEAPU32, HEAPF32, HEAPF64;

            function updateGlobalBufferAndViews(buf) {
                buffer = buf;
                Module["HEAP8"] = HEAP8 = new Int8Array(buf);
                Module["HEAP16"] = HEAP16 = new Int16Array(buf);
                Module["HEAP32"] = HEAP32 = new Int32Array(buf);
                Module["HEAPU8"] = HEAPU8 = new Uint8Array(buf);
                Module["HEAPU16"] = HEAPU16 = new Uint16Array(buf);
                Module["HEAPU32"] = HEAPU32 = new Uint32Array(buf);
                Module["HEAPF32"] = HEAPF32 = new Float32Array(buf);
                Module["HEAPF64"] = HEAPF64 = new Float64Array(buf)
            }
            var TOTAL_STACK = 5242880;
            if (Module["TOTAL_STACK"]) assert(TOTAL_STACK === Module["TOTAL_STACK"], "the stack size can no longer be determined at runtime");
            var INITIAL_MEMORY = Module["INITIAL_MEMORY"] || 16777216;
            if (!Object.getOwnPropertyDescriptor(Module, "INITIAL_MEMORY")) {
                Object.defineProperty(Module, "INITIAL_MEMORY", {
                    configurable: true,
                    get: function() {
                        abort("Module.INITIAL_MEMORY has been replaced with plain INITIAL_MEMORY (the initial value can be provided on Module, but after startup the value is only looked for on a local variable of that name)")
                    }
                })
            }
            assert(INITIAL_MEMORY >= TOTAL_STACK, "INITIAL_MEMORY should be larger than TOTAL_STACK, was " + INITIAL_MEMORY + "! (TOTAL_STACK=" + TOTAL_STACK + ")");
            assert(typeof Int32Array !== "undefined" && typeof Float64Array !== "undefined" && Int32Array.prototype.subarray !== undefined && Int32Array.prototype.set !== undefined, "JS engine does not provide full typed array support");
            assert(!Module["wasmMemory"], "Use of `wasmMemory` detected.  Use -s IMPORTED_MEMORY to define wasmMemory externally");
            assert(INITIAL_MEMORY == 16777216, "Detected runtime INITIAL_MEMORY setting.  Use -s IMPORTED_MEMORY to define wasmMemory dynamically");
            var wasmTable;

            function writeStackCookie() {
                var max = _emscripten_stack_get_end();
                assert((max & 3) == 0);
                HEAP32[max + 4 >> 2] = 34821223;
                HEAP32[max + 8 >> 2] = 2310721022;
                HEAP32[0] = 1668509029
            }

            function checkStackCookie() {
                if (ABORT) return;
                var max = _emscripten_stack_get_end();
                var cookie1 = HEAPU32[max + 4 >> 2];
                var cookie2 = HEAPU32[max + 8 >> 2];
                if (cookie1 != 34821223 || cookie2 != 2310721022) {
                    abort("Stack overflow! Stack cookie has been overwritten, expected hex dwords 0x89BACDFE and 0x2135467, but received 0x" + cookie2.toString(16) + " 0x" + cookie1.toString(16))
                }
                if (HEAP32[0] !== 1668509029) abort("Runtime error: The application has corrupted its heap memory area (address zero)!")
            }(function() {
                var h16 = new Int16Array(1);
                var h8 = new Int8Array(h16.buffer);
                h16[0] = 25459;
                if (h8[0] !== 115 || h8[1] !== 99) throw "Runtime error: expected the system to be little-endian! (Run with -s SUPPORT_BIG_ENDIAN=1 to bypass)"
            })();
            var __ATPRERUN__ = [];
            var __ATINIT__ = [];
            var __ATEXIT__ = [];
            var __ATPOSTRUN__ = [];
            var runtimeInitialized = false;
            var runtimeExited = false;
            var runtimeKeepaliveCounter = 0;

            function keepRuntimeAlive() {
                return noExitRuntime || runtimeKeepaliveCounter > 0
            }

            function preRun() {
                if (Module["preRun"]) {
                    if (typeof Module["preRun"] == "function") Module["preRun"] = [Module["preRun"]];
                    while (Module["preRun"].length) {
                        addOnPreRun(Module["preRun"].shift())
                    }
                }
                callRuntimeCallbacks(__ATPRERUN__)
            }

            function initRuntime() {
                checkStackCookie();
                assert(!runtimeInitialized);
                runtimeInitialized = true;
                if (!Module["noFSInit"] && !FS.init.initialized) FS.init();
                FS.ignorePermissions = false;
                TTY.init();
                callRuntimeCallbacks(__ATINIT__)
            }

            function exitRuntime() {
                checkStackCookie();
                ___funcs_on_exit();
                callRuntimeCallbacks(__ATEXIT__);
                FS.quit();
                TTY.shutdown();
                runtimeExited = true
            }

            function postRun() {
                checkStackCookie();
                if (Module["postRun"]) {
                    if (typeof Module["postRun"] == "function") Module["postRun"] = [Module["postRun"]];
                    while (Module["postRun"].length) {
                        addOnPostRun(Module["postRun"].shift())
                    }
                }
                callRuntimeCallbacks(__ATPOSTRUN__)
            }

            function addOnPreRun(cb) {
                __ATPRERUN__.unshift(cb)
            }

            function addOnInit(cb) {
                __ATINIT__.unshift(cb)
            }

            function addOnPostRun(cb) {
                __ATPOSTRUN__.unshift(cb)
            }
            assert(Math.imul, "This browser does not support Math.imul(), build with LEGACY_VM_SUPPORT or POLYFILL_OLD_MATH_FUNCTIONS to add in a polyfill");
            assert(Math.fround, "This browser does not support Math.fround(), build with LEGACY_VM_SUPPORT or POLYFILL_OLD_MATH_FUNCTIONS to add in a polyfill");
            assert(Math.clz32, "This browser does not support Math.clz32(), build with LEGACY_VM_SUPPORT or POLYFILL_OLD_MATH_FUNCTIONS to add in a polyfill");
            assert(Math.trunc, "This browser does not support Math.trunc(), build with LEGACY_VM_SUPPORT or POLYFILL_OLD_MATH_FUNCTIONS to add in a polyfill");
            var runDependencies = 0;
            var runDependencyWatcher = null;
            var dependenciesFulfilled = null;
            var runDependencyTracking = {};

            function getUniqueRunDependency(id) {
                var orig = id;
                while (1) {
                    if (!runDependencyTracking[id]) return id;
                    id = orig + Math.random()
                }
            }

            function addRunDependency(id) {
                runDependencies++;
                if (Module["monitorRunDependencies"]) {
                    Module["monitorRunDependencies"](runDependencies)
                }
                if (id) {
                    assert(!runDependencyTracking[id]);
                    runDependencyTracking[id] = 1;
                    if (runDependencyWatcher === null && typeof setInterval !== "undefined") {
                        runDependencyWatcher = setInterval(function() {
                            if (ABORT) {
                                clearInterval(runDependencyWatcher);
                                runDependencyWatcher = null;
                                return
                            }
                            var shown = false;
                            for (var dep in runDependencyTracking) {
                                if (!shown) {
                                    shown = true;
                                    err("still waiting on run dependencies:")
                                }
                                err("dependency: " + dep)
                            }
                            if (shown) {
                                err("(end of list)")
                            }
                        }, 1e4)
                    }
                } else {
                    err("warning: run dependency added without ID")
                }
            }

            function removeRunDependency(id) {
                runDependencies--;
                if (Module["monitorRunDependencies"]) {
                    Module["monitorRunDependencies"](runDependencies)
                }
                if (id) {
                    assert(runDependencyTracking[id]);
                    delete runDependencyTracking[id]
                } else {
                    err("warning: run dependency removed without ID")
                }
                if (runDependencies == 0) {
                    if (runDependencyWatcher !== null) {
                        clearInterval(runDependencyWatcher);
                        runDependencyWatcher = null
                    }
                    if (dependenciesFulfilled) {
                        var callback = dependenciesFulfilled;
                        dependenciesFulfilled = null;
                        callback()
                    }
                }
            }
            Module["preloadedImages"] = {};
            Module["preloadedAudios"] = {};

            function abort(what) {
                {
                    if (Module["onAbort"]) {
                        Module["onAbort"](what)
                    }
                }
                what = "Aborted(" + what + ")";
                err(what);
                ABORT = true;
                EXITSTATUS = 1;
                var e = new WebAssembly.RuntimeError(what);
                readyPromiseReject(e);
                throw e
            }
            var dataURIPrefix = "data:application/octet-stream;base64,";

            function isDataURI(filename) {
                return filename.startsWith(dataURIPrefix)
            }

            function isFileURI(filename) {
                return filename.startsWith("file://")
            }

            function createExportWrapper(name, fixedasm) {
                return function() {
                    var displayName = name;
                    var asm = fixedasm;
                    if (!fixedasm) {
                        asm = Module["asm"]
                    }
                    assert(runtimeInitialized, "native function `" + displayName + "` called before runtime initialization");
                    assert(!runtimeExited, "native function `" + displayName + "` called after runtime exit (use NO_EXIT_RUNTIME to keep it alive after main() exits)");
                    if (!asm[name]) {
                        assert(asm[name], "exported native function `" + displayName + "` not found")
                    }
                    return asm[name].apply(null, arguments)
                }
            }
            var wasmBinaryFile;
            wasmBinaryFile = "swipl-web.wasm";
            if (!isDataURI(wasmBinaryFile)) {
                wasmBinaryFile = locateFile(wasmBinaryFile)
            }

            function getBinary(file) {
                try {
                    if (file == wasmBinaryFile && wasmBinary) {
                        return new Uint8Array(wasmBinary)
                    }
                    if (readBinary) {
                        return readBinary(file)
                    } else {
                        throw "both async and sync fetching of the wasm failed"
                    }
                } catch (err) {
                    abort(err)
                }
            }

            function getBinaryPromise() {
                if (!wasmBinary && (ENVIRONMENT_IS_WEB || ENVIRONMENT_IS_WORKER)) {
                    if (typeof fetch === "function" && !isFileURI(wasmBinaryFile)) {
                        return fetch(wasmBinaryFile, {
                            credentials: "same-origin"
                        }).then(function(response) {
                            if (!response["ok"]) {
                                throw "failed to load wasm binary file at '" + wasmBinaryFile + "'"
                            }
                            return response["arrayBuffer"]()
                        }).catch(function() {
                            return getBinary(wasmBinaryFile)
                        })
                    } else {
                        if (readAsync) {
                            return new Promise(function(resolve, reject) {
                                readAsync(wasmBinaryFile, function(response) {
                                    resolve(new Uint8Array(response))
                                }, reject)
                            })
                        }
                    }
                }
                return Promise.resolve().then(function() {
                    return getBinary(wasmBinaryFile)
                })
            }

            function createWasm() {
                var info = {
                    "env": asmLibraryArg,
                    "wasi_snapshot_preview1": asmLibraryArg
                };

                function receiveInstance(instance, module) {
                    var exports = instance.exports;
                    Module["asm"] = exports;
                    wasmMemory = Module["asm"]["memory"];
                    assert(wasmMemory, "memory not found in wasm exports");
                    updateGlobalBufferAndViews(wasmMemory.buffer);
                    wasmTable = Module["asm"]["__indirect_function_table"];
                    assert(wasmTable, "table not found in wasm exports");
                    addOnInit(Module["asm"]["__wasm_call_ctors"]);
                    removeRunDependency("wasm-instantiate")
                }
                addRunDependency("wasm-instantiate");
                var trueModule = Module;

                function receiveInstantiationResult(result) {
                    assert(Module === trueModule, "the Module object should not be replaced during async compilation - perhaps the order of HTML elements is wrong?");
                    trueModule = null;
                    receiveInstance(result["instance"])
                }

                function instantiateArrayBuffer(receiver) {
                    return getBinaryPromise().then(function(binary) {
                        return WebAssembly.instantiate(binary, info)
                    }).then(function(instance) {
                        return instance
                    }).then(receiver, function(reason) {
                        err("failed to asynchronously prepare wasm: " + reason);
                        if (isFileURI(wasmBinaryFile)) {
                            err("warning: Loading from a file URI (" + wasmBinaryFile + ") is not supported in most browsers. See https://emscripten.org/docs/getting_started/FAQ.html#how-do-i-run-a-local-webserver-for-testing-why-does-my-program-stall-in-downloading-or-preparing")
                        }
                        abort(reason)
                    })
                }

                function instantiateAsync() {
                    if (!wasmBinary && typeof WebAssembly.instantiateStreaming === "function" && !isDataURI(wasmBinaryFile) && !isFileURI(wasmBinaryFile) && typeof fetch === "function") {
                        return fetch(wasmBinaryFile, {
                            credentials: "same-origin"
                        }).then(function(response) {
                            var result = WebAssembly.instantiateStreaming(response, info);
                            return result.then(receiveInstantiationResult, function(reason) {
                                err("wasm streaming compile failed: " + reason);
                                err("falling back to ArrayBuffer instantiation");
                                return instantiateArrayBuffer(receiveInstantiationResult)
                            })
                        })
                    } else {
                        return instantiateArrayBuffer(receiveInstantiationResult)
                    }
                }
                if (Module["instantiateWasm"]) {
                    try {
                        var exports = Module["instantiateWasm"](info, receiveInstance);
                        return exports
                    } catch (e) {
                        err("Module.instantiateWasm callback failed with error: " + e);
                        return false
                    }
                }
                instantiateAsync().catch(readyPromiseReject);
                return {}
            }
            var tempDouble;
            var tempI64;

            function callRuntimeCallbacks(callbacks) {
                while (callbacks.length > 0) {
                    var callback = callbacks.shift();
                    if (typeof callback == "function") {
                        callback(Module);
                        continue
                    }
                    var func = callback.func;
                    if (typeof func === "number") {
                        if (callback.arg === undefined) {
                            getWasmTableEntry(func)()
                        } else {
                            getWasmTableEntry(func)(callback.arg)
                        }
                    } else {
                        func(callback.arg === undefined ? null : callback.arg)
                    }
                }
            }

            function demangle(func) {
                warnOnce("warning: build with  -s DEMANGLE_SUPPORT=1  to link in libcxxabi demangling");
                return func
            }

            function demangleAll(text) {
                var regex = /\b_Z[\w\d_]+/g;
                return text.replace(regex, function(x) {
                    var y = demangle(x);
                    return x === y ? x : y + " [" + x + "]"
                })
            }
            var wasmTableMirror = [];

            function getWasmTableEntry(funcPtr) {
                var func = wasmTableMirror[funcPtr];
                if (!func) {
                    if (funcPtr >= wasmTableMirror.length) wasmTableMirror.length = funcPtr + 1;
                    wasmTableMirror[funcPtr] = func = wasmTable.get(funcPtr)
                }
                assert(wasmTable.get(funcPtr) == func, "JavaScript-side Wasm function table mirror is out of date!");
                return func
            }

            function jsStackTrace() {
                var error = new Error;
                if (!error.stack) {
                    try {
                        throw new Error
                    } catch (e) {
                        error = e
                    }
                    if (!error.stack) {
                        return "(no stack trace available)"
                    }
                }
                return error.stack.toString()
            }

            function setWasmTableEntry(idx, func) {
                wasmTable.set(idx, func);
                wasmTableMirror[idx] = func
            }
            var PATH = {
                splitPath: function(filename) {
                    var splitPathRe = /^(\/?|)([\s\S]*?)((?:\.{1,2}|[^\/]+?|)(\.[^.\/]*|))(?:[\/]*)$/;
                    return splitPathRe.exec(filename).slice(1)
                },
                normalizeArray: function(parts, allowAboveRoot) {
                    var up = 0;
                    for (var i = parts.length - 1; i >= 0; i--) {
                        var last = parts[i];
                        if (last === ".") {
                            parts.splice(i, 1)
                        } else if (last === "..") {
                            parts.splice(i, 1);
                            up++
                        } else if (up) {
                            parts.splice(i, 1);
                            up--
                        }
                    }
                    if (allowAboveRoot) {
                        for (; up; up--) {
                            parts.unshift("..")
                        }
                    }
                    return parts
                },
                normalize: function(path) {
                    var isAbsolute = path.charAt(0) === "/",
                        trailingSlash = path.substr(-1) === "/";
                    path = PATH.normalizeArray(path.split("/").filter(function(p) {
                        return !!p
                    }), !isAbsolute).join("/");
                    if (!path && !isAbsolute) {
                        path = "."
                    }
                    if (path && trailingSlash) {
                        path += "/"
                    }
                    return (isAbsolute ? "/" : "") + path
                },
                dirname: function(path) {
                    var result = PATH.splitPath(path),
                        root = result[0],
                        dir = result[1];
                    if (!root && !dir) {
                        return "."
                    }
                    if (dir) {
                        dir = dir.substr(0, dir.length - 1)
                    }
                    return root + dir
                },
                basename: function(path) {
                    if (path === "/") return "/";
                    path = PATH.normalize(path);
                    path = path.replace(/\/$/, "");
                    var lastSlash = path.lastIndexOf("/");
                    if (lastSlash === -1) return path;
                    return path.substr(lastSlash + 1)
                },
                extname: function(path) {
                    return PATH.splitPath(path)[3]
                },
                join: function() {
                    var paths = Array.prototype.slice.call(arguments, 0);
                    return PATH.normalize(paths.join("/"))
                },
                join2: function(l, r) {
                    return PATH.normalize(l + "/" + r)
                }
            };

            function getRandomDevice() {
                if (typeof crypto === "object" && typeof crypto["getRandomValues"] === "function") {
                    var randomBuffer = new Uint8Array(1);
                    return function() {
                        crypto.getRandomValues(randomBuffer);
                        return randomBuffer[0]
                    }
                } else if (ENVIRONMENT_IS_NODE) {
                    try {
                        var crypto_module = require("crypto");
                        return function() {
                            return crypto_module["randomBytes"](1)[0]
                        }
                    } catch (e) {}
                }
                return function() {
                    abort("no cryptographic support found for randomDevice. consider polyfilling it if you want to use something insecure like Math.random(), e.g. put this in a --pre-js: var crypto = { getRandomValues: function(array) { for (var i = 0; i < array.length; i++) array[i] = (Math.random()*256)|0 } };")
                }
            }
            var PATH_FS = {
                resolve: function() {
                    var resolvedPath = "",
                        resolvedAbsolute = false;
                    for (var i = arguments.length - 1; i >= -1 && !resolvedAbsolute; i--) {
                        var path = i >= 0 ? arguments[i] : FS.cwd();
                        if (typeof path !== "string") {
                            throw new TypeError("Arguments to path.resolve must be strings")
                        } else if (!path) {
                            return ""
                        }
                        resolvedPath = path + "/" + resolvedPath;
                        resolvedAbsolute = path.charAt(0) === "/"
                    }
                    resolvedPath = PATH.normalizeArray(resolvedPath.split("/").filter(function(p) {
                        return !!p
                    }), !resolvedAbsolute).join("/");
                    return (resolvedAbsolute ? "/" : "") + resolvedPath || "."
                },
                relative: function(from, to) {
                    from = PATH_FS.resolve(from).substr(1);
                    to = PATH_FS.resolve(to).substr(1);

                    function trim(arr) {
                        var start = 0;
                        for (; start < arr.length; start++) {
                            if (arr[start] !== "") break
                        }
                        var end = arr.length - 1;
                        for (; end >= 0; end--) {
                            if (arr[end] !== "") break
                        }
                        if (start > end) return [];
                        return arr.slice(start, end - start + 1)
                    }
                    var fromParts = trim(from.split("/"));
                    var toParts = trim(to.split("/"));
                    var length = Math.min(fromParts.length, toParts.length);
                    var samePartsLength = length;
                    for (var i = 0; i < length; i++) {
                        if (fromParts[i] !== toParts[i]) {
                            samePartsLength = i;
                            break
                        }
                    }
                    var outputParts = [];
                    for (var i = samePartsLength; i < fromParts.length; i++) {
                        outputParts.push("..")
                    }
                    outputParts = outputParts.concat(toParts.slice(samePartsLength));
                    return outputParts.join("/")
                }
            };
            var TTY = {
                ttys: [],
                init: function() {},
                shutdown: function() {},
                register: function(dev, ops) {
                    TTY.ttys[dev] = {
                        input: [],
                        output: [],
                        ops: ops
                    };
                    FS.registerDevice(dev, TTY.stream_ops)
                },
                stream_ops: {
                    open: function(stream) {
                        var tty = TTY.ttys[stream.node.rdev];
                        if (!tty) {
                            throw new FS.ErrnoError(43)
                        }
                        stream.tty = tty;
                        stream.seekable = false
                    },
                    close: function(stream) {
                        stream.tty.ops.flush(stream.tty)
                    },
                    flush: function(stream) {
                        stream.tty.ops.flush(stream.tty)
                    },
                    read: function(stream, buffer, offset, length, pos) {
                        if (!stream.tty || !stream.tty.ops.get_char) {
                            throw new FS.ErrnoError(60)
                        }
                        var bytesRead = 0;
                        for (var i = 0; i < length; i++) {
                            var result;
                            try {
                                result = stream.tty.ops.get_char(stream.tty)
                            } catch (e) {
                                throw new FS.ErrnoError(29)
                            }
                            if (result === undefined && bytesRead === 0) {
                                throw new FS.ErrnoError(6)
                            }
                            if (result === null || result === undefined) break;
                            bytesRead++;
                            buffer[offset + i] = result
                        }
                        if (bytesRead) {
                            stream.node.timestamp = Date.now()
                        }
                        return bytesRead
                    },
                    write: function(stream, buffer, offset, length, pos) {
                        if (!stream.tty || !stream.tty.ops.put_char) {
                            throw new FS.ErrnoError(60)
                        }
                        try {
                            for (var i = 0; i < length; i++) {
                                stream.tty.ops.put_char(stream.tty, buffer[offset + i])
                            }
                        } catch (e) {
                            throw new FS.ErrnoError(29)
                        }
                        if (length) {
                            stream.node.timestamp = Date.now()
                        }
                        return i
                    }
                },
                default_tty_ops: {
                    get_char: function(tty) {
                        if (!tty.input.length) {
                            var result = null;
                            if (ENVIRONMENT_IS_NODE) {
                                var BUFSIZE = 256;
                                var buf = Buffer.alloc(BUFSIZE);
                                var bytesRead = 0;
                                try {
                                    bytesRead = fs.readSync(process.stdin.fd, buf, 0, BUFSIZE, -1)
                                } catch (e) {
                                    if (e.toString().includes("EOF")) bytesRead = 0;
                                    else throw e
                                }
                                if (bytesRead > 0) {
                                    result = buf.slice(0, bytesRead).toString("utf-8")
                                } else {
                                    result = null
                                }
                            } else if (typeof window != "undefined" && typeof window.prompt == "function") {
                                result = window.prompt("Input: ");
                                if (result !== null) {
                                    result += "\n"
                                }
                            } else if (typeof readline == "function") {
                                result = readline();
                                if (result !== null) {
                                    result += "\n"
                                }
                            }
                            if (!result) {
                                return null
                            }
                            tty.input = intArrayFromString(result, true)
                        }
                        return tty.input.shift()
                    },
                    put_char: function(tty, val) {
                        if (val === null || val === 10) {
                            out(UTF8ArrayToString(tty.output, 0));
                            tty.output = []
                        } else {
                            if (val != 0) tty.output.push(val)
                        }
                    },
                    flush: function(tty) {
                        if (tty.output && tty.output.length > 0) {
                            out(UTF8ArrayToString(tty.output, 0));
                            tty.output = []
                        }
                    }
                },
                default_tty1_ops: {
                    put_char: function(tty, val) {
                        if (val === null || val === 10) {
                            err(UTF8ArrayToString(tty.output, 0));
                            tty.output = []
                        } else {
                            if (val != 0) tty.output.push(val)
                        }
                    },
                    flush: function(tty) {
                        if (tty.output && tty.output.length > 0) {
                            err(UTF8ArrayToString(tty.output, 0));
                            tty.output = []
                        }
                    }
                }
            };

            function zeroMemory(address, size) {
                HEAPU8.fill(0, address, address + size)
            }

            function alignMemory(size, alignment) {
                assert(alignment, "alignment argument is required");
                return Math.ceil(size / alignment) * alignment
            }

            function mmapAlloc(size) {
                size = alignMemory(size, 65536);
                var ptr = _memalign(65536, size);
                if (!ptr) return 0;
                zeroMemory(ptr, size);
                return ptr
            }
            var MEMFS = {
                ops_table: null,
                mount: function(mount) {
                    return MEMFS.createNode(null, "/", 16384 | 511, 0)
                },
                createNode: function(parent, name, mode, dev) {
                    if (FS.isBlkdev(mode) || FS.isFIFO(mode)) {
                        throw new FS.ErrnoError(63)
                    }
                    if (!MEMFS.ops_table) {
                        MEMFS.ops_table = {
                            dir: {
                                node: {
                                    getattr: MEMFS.node_ops.getattr,
                                    setattr: MEMFS.node_ops.setattr,
                                    lookup: MEMFS.node_ops.lookup,
                                    mknod: MEMFS.node_ops.mknod,
                                    rename: MEMFS.node_ops.rename,
                                    unlink: MEMFS.node_ops.unlink,
                                    rmdir: MEMFS.node_ops.rmdir,
                                    readdir: MEMFS.node_ops.readdir,
                                    symlink: MEMFS.node_ops.symlink
                                },
                                stream: {
                                    llseek: MEMFS.stream_ops.llseek
                                }
                            },
                            file: {
                                node: {
                                    getattr: MEMFS.node_ops.getattr,
                                    setattr: MEMFS.node_ops.setattr
                                },
                                stream: {
                                    llseek: MEMFS.stream_ops.llseek,
                                    read: MEMFS.stream_ops.read,
                                    write: MEMFS.stream_ops.write,
                                    allocate: MEMFS.stream_ops.allocate,
                                    mmap: MEMFS.stream_ops.mmap,
                                    msync: MEMFS.stream_ops.msync
                                }
                            },
                            link: {
                                node: {
                                    getattr: MEMFS.node_ops.getattr,
                                    setattr: MEMFS.node_ops.setattr,
                                    readlink: MEMFS.node_ops.readlink
                                },
                                stream: {}
                            },
                            chrdev: {
                                node: {
                                    getattr: MEMFS.node_ops.getattr,
                                    setattr: MEMFS.node_ops.setattr
                                },
                                stream: FS.chrdev_stream_ops
                            }
                        }
                    }
                    var node = FS.createNode(parent, name, mode, dev);
                    if (FS.isDir(node.mode)) {
                        node.node_ops = MEMFS.ops_table.dir.node;
                        node.stream_ops = MEMFS.ops_table.dir.stream;
                        node.contents = {}
                    } else if (FS.isFile(node.mode)) {
                        node.node_ops = MEMFS.ops_table.file.node;
                        node.stream_ops = MEMFS.ops_table.file.stream;
                        node.usedBytes = 0;
                        node.contents = null
                    } else if (FS.isLink(node.mode)) {
                        node.node_ops = MEMFS.ops_table.link.node;
                        node.stream_ops = MEMFS.ops_table.link.stream
                    } else if (FS.isChrdev(node.mode)) {
                        node.node_ops = MEMFS.ops_table.chrdev.node;
                        node.stream_ops = MEMFS.ops_table.chrdev.stream
                    }
                    node.timestamp = Date.now();
                    if (parent) {
                        parent.contents[name] = node;
                        parent.timestamp = node.timestamp
                    }
                    return node
                },
                getFileDataAsTypedArray: function(node) {
                    if (!node.contents) return new Uint8Array(0);
                    if (node.contents.subarray) return node.contents.subarray(0, node.usedBytes);
                    return new Uint8Array(node.contents)
                },
                expandFileStorage: function(node, newCapacity) {
                    var prevCapacity = node.contents ? node.contents.length : 0;
                    if (prevCapacity >= newCapacity) return;
                    var CAPACITY_DOUBLING_MAX = 1024 * 1024;
                    newCapacity = Math.max(newCapacity, prevCapacity * (prevCapacity < CAPACITY_DOUBLING_MAX ? 2 : 1.125) >>> 0);
                    if (prevCapacity != 0) newCapacity = Math.max(newCapacity, 256);
                    var oldContents = node.contents;
                    node.contents = new Uint8Array(newCapacity);
                    if (node.usedBytes > 0) node.contents.set(oldContents.subarray(0, node.usedBytes), 0)
                },
                resizeFileStorage: function(node, newSize) {
                    if (node.usedBytes == newSize) return;
                    if (newSize == 0) {
                        node.contents = null;
                        node.usedBytes = 0
                    } else {
                        var oldContents = node.contents;
                        node.contents = new Uint8Array(newSize);
                        if (oldContents) {
                            node.contents.set(oldContents.subarray(0, Math.min(newSize, node.usedBytes)))
                        }
                        node.usedBytes = newSize
                    }
                },
                node_ops: {
                    getattr: function(node) {
                        var attr = {};
                        attr.dev = FS.isChrdev(node.mode) ? node.id : 1;
                        attr.ino = node.id;
                        attr.mode = node.mode;
                        attr.nlink = 1;
                        attr.uid = 0;
                        attr.gid = 0;
                        attr.rdev = node.rdev;
                        if (FS.isDir(node.mode)) {
                            attr.size = 4096
                        } else if (FS.isFile(node.mode)) {
                            attr.size = node.usedBytes
                        } else if (FS.isLink(node.mode)) {
                            attr.size = node.link.length
                        } else {
                            attr.size = 0
                        }
                        attr.atime = new Date(node.timestamp);
                        attr.mtime = new Date(node.timestamp);
                        attr.ctime = new Date(node.timestamp);
                        attr.blksize = 4096;
                        attr.blocks = Math.ceil(attr.size / attr.blksize);
                        return attr
                    },
                    setattr: function(node, attr) {
                        if (attr.mode !== undefined) {
                            node.mode = attr.mode
                        }
                        if (attr.timestamp !== undefined) {
                            node.timestamp = attr.timestamp
                        }
                        if (attr.size !== undefined) {
                            MEMFS.resizeFileStorage(node, attr.size)
                        }
                    },
                    lookup: function(parent, name) {
                        throw FS.genericErrors[44]
                    },
                    mknod: function(parent, name, mode, dev) {
                        return MEMFS.createNode(parent, name, mode, dev)
                    },
                    rename: function(old_node, new_dir, new_name) {
                        if (FS.isDir(old_node.mode)) {
                            var new_node;
                            try {
                                new_node = FS.lookupNode(new_dir, new_name)
                            } catch (e) {}
                            if (new_node) {
                                for (var i in new_node.contents) {
                                    throw new FS.ErrnoError(55)
                                }
                            }
                        }
                        delete old_node.parent.contents[old_node.name];
                        old_node.parent.timestamp = Date.now();
                        old_node.name = new_name;
                        new_dir.contents[new_name] = old_node;
                        new_dir.timestamp = old_node.parent.timestamp;
                        old_node.parent = new_dir
                    },
                    unlink: function(parent, name) {
                        delete parent.contents[name];
                        parent.timestamp = Date.now()
                    },
                    rmdir: function(parent, name) {
                        var node = FS.lookupNode(parent, name);
                        for (var i in node.contents) {
                            throw new FS.ErrnoError(55)
                        }
                        delete parent.contents[name];
                        parent.timestamp = Date.now()
                    },
                    readdir: function(node) {
                        var entries = [".", ".."];
                        for (var key in node.contents) {
                            if (!node.contents.hasOwnProperty(key)) {
                                continue
                            }
                            entries.push(key)
                        }
                        return entries
                    },
                    symlink: function(parent, newname, oldpath) {
                        var node = MEMFS.createNode(parent, newname, 511 | 40960, 0);
                        node.link = oldpath;
                        return node
                    },
                    readlink: function(node) {
                        if (!FS.isLink(node.mode)) {
                            throw new FS.ErrnoError(28)
                        }
                        return node.link
                    }
                },
                stream_ops: {
                    read: function(stream, buffer, offset, length, position) {
                        var contents = stream.node.contents;
                        if (position >= stream.node.usedBytes) return 0;
                        var size = Math.min(stream.node.usedBytes - position, length);
                        assert(size >= 0);
                        if (size > 8 && contents.subarray) {
                            buffer.set(contents.subarray(position, position + size), offset)
                        } else {
                            for (var i = 0; i < size; i++) buffer[offset + i] = contents[position + i]
                        }
                        return size
                    },
                    write: function(stream, buffer, offset, length, position, canOwn) {
                        assert(!(buffer instanceof ArrayBuffer));
                        if (buffer.buffer === HEAP8.buffer) {
                            canOwn = false
                        }
                        if (!length) return 0;
                        var node = stream.node;
                        node.timestamp = Date.now();
                        if (buffer.subarray && (!node.contents || node.contents.subarray)) {
                            if (canOwn) {
                                assert(position === 0, "canOwn must imply no weird position inside the file");
                                node.contents = buffer.subarray(offset, offset + length);
                                node.usedBytes = length;
                                return length
                            } else if (node.usedBytes === 0 && position === 0) {
                                node.contents = buffer.slice(offset, offset + length);
                                node.usedBytes = length;
                                return length
                            } else if (position + length <= node.usedBytes) {
                                node.contents.set(buffer.subarray(offset, offset + length), position);
                                return length
                            }
                        }
                        MEMFS.expandFileStorage(node, position + length);
                        if (node.contents.subarray && buffer.subarray) {
                            node.contents.set(buffer.subarray(offset, offset + length), position)
                        } else {
                            for (var i = 0; i < length; i++) {
                                node.contents[position + i] = buffer[offset + i]
                            }
                        }
                        node.usedBytes = Math.max(node.usedBytes, position + length);
                        return length
                    },
                    llseek: function(stream, offset, whence) {
                        var position = offset;
                        if (whence === 1) {
                            position += stream.position
                        } else if (whence === 2) {
                            if (FS.isFile(stream.node.mode)) {
                                position += stream.node.usedBytes
                            }
                        }
                        if (position < 0) {
                            throw new FS.ErrnoError(28)
                        }
                        return position
                    },
                    allocate: function(stream, offset, length) {
                        MEMFS.expandFileStorage(stream.node, offset + length);
                        stream.node.usedBytes = Math.max(stream.node.usedBytes, offset + length)
                    },
                    mmap: function(stream, address, length, position, prot, flags) {
                        if (address !== 0) {
                            throw new FS.ErrnoError(28)
                        }
                        if (!FS.isFile(stream.node.mode)) {
                            throw new FS.ErrnoError(43)
                        }
                        var ptr;
                        var allocated;
                        var contents = stream.node.contents;
                        if (!(flags & 2) && contents.buffer === buffer) {
                            allocated = false;
                            ptr = contents.byteOffset
                        } else {
                            if (position > 0 || position + length < contents.length) {
                                if (contents.subarray) {
                                    contents = contents.subarray(position, position + length)
                                } else {
                                    contents = Array.prototype.slice.call(contents, position, position + length)
                                }
                            }
                            allocated = true;
                            ptr = mmapAlloc(length);
                            if (!ptr) {
                                throw new FS.ErrnoError(48)
                            }
                            HEAP8.set(contents, ptr)
                        }
                        return {
                            ptr: ptr,
                            allocated: allocated
                        }
                    },
                    msync: function(stream, buffer, offset, length, mmapFlags) {
                        if (!FS.isFile(stream.node.mode)) {
                            throw new FS.ErrnoError(43)
                        }
                        if (mmapFlags & 2) {
                            return 0
                        }
                        var bytesWritten = MEMFS.stream_ops.write(stream, buffer, 0, length, offset, false);
                        return 0
                    }
                }
            };

            function asyncLoad(url, onload, onerror, noRunDep) {
                var dep = !noRunDep ? getUniqueRunDependency("al " + url) : "";
                readAsync(url, function(arrayBuffer) {
                    assert(arrayBuffer, 'Loading data file "' + url + '" failed (no arrayBuffer).');
                    onload(new Uint8Array(arrayBuffer));
                    if (dep) removeRunDependency(dep)
                }, function(event) {
                    if (onerror) {
                        onerror()
                    } else {
                        throw 'Loading data file "' + url + '" failed.'
                    }
                });
                if (dep) addRunDependency(dep)
            }
            var ERRNO_MESSAGES = {
                0: "Success",
                1: "Arg list too long",
                2: "Permission denied",
                3: "Address already in use",
                4: "Address not available",
                5: "Address family not supported by protocol family",
                6: "No more processes",
                7: "Socket already connected",
                8: "Bad file number",
                9: "Trying to read unreadable message",
                10: "Mount device busy",
                11: "Operation canceled",
                12: "No children",
                13: "Connection aborted",
                14: "Connection refused",
                15: "Connection reset by peer",
                16: "File locking deadlock error",
                17: "Destination address required",
                18: "Math arg out of domain of func",
                19: "Quota exceeded",
                20: "File exists",
                21: "Bad address",
                22: "File too large",
                23: "Host is unreachable",
                24: "Identifier removed",
                25: "Illegal byte sequence",
                26: "Connection already in progress",
                27: "Interrupted system call",
                28: "Invalid argument",
                29: "I/O error",
                30: "Socket is already connected",
                31: "Is a directory",
                32: "Too many symbolic links",
                33: "Too many open files",
                34: "Too many links",
                35: "Message too long",
                36: "Multihop attempted",
                37: "File or path name too long",
                38: "Network interface is not configured",
                39: "Connection reset by network",
                40: "Network is unreachable",
                41: "Too many open files in system",
                42: "No buffer space available",
                43: "No such device",
                44: "No such file or directory",
                45: "Exec format error",
                46: "No record locks available",
                47: "The link has been severed",
                48: "Not enough core",
                49: "No message of desired type",
                50: "Protocol not available",
                51: "No space left on device",
                52: "Function not implemented",
                53: "Socket is not connected",
                54: "Not a directory",
                55: "Directory not empty",
                56: "State not recoverable",
                57: "Socket operation on non-socket",
                59: "Not a typewriter",
                60: "No such device or address",
                61: "Value too large for defined data type",
                62: "Previous owner died",
                63: "Not super-user",
                64: "Broken pipe",
                65: "Protocol error",
                66: "Unknown protocol",
                67: "Protocol wrong type for socket",
                68: "Math result not representable",
                69: "Read only file system",
                70: "Illegal seek",
                71: "No such process",
                72: "Stale file handle",
                73: "Connection timed out",
                74: "Text file busy",
                75: "Cross-device link",
                100: "Device not a stream",
                101: "Bad font file fmt",
                102: "Invalid slot",
                103: "Invalid request code",
                104: "No anode",
                105: "Block device required",
                106: "Channel number out of range",
                107: "Level 3 halted",
                108: "Level 3 reset",
                109: "Link number out of range",
                110: "Protocol driver not attached",
                111: "No CSI structure available",
                112: "Level 2 halted",
                113: "Invalid exchange",
                114: "Invalid request descriptor",
                115: "Exchange full",
                116: "No data (for no delay io)",
                117: "Timer expired",
                118: "Out of streams resources",
                119: "Machine is not on the network",
                120: "Package not installed",
                121: "The object is remote",
                122: "Advertise error",
                123: "Srmount error",
                124: "Communication error on send",
                125: "Cross mount point (not really error)",
                126: "Given log. name not unique",
                127: "f.d. invalid for this operation",
                128: "Remote address changed",
                129: "Can   access a needed shared lib",
                130: "Accessing a corrupted shared lib",
                131: ".lib section in a.out corrupted",
                132: "Attempting to link in too many libs",
                133: "Attempting to exec a shared library",
                135: "Streams pipe error",
                136: "Too many users",
                137: "Socket type not supported",
                138: "Not supported",
                139: "Protocol family not supported",
                140: "Can't send after socket shutdown",
                141: "Too many references",
                142: "Host is down",
                148: "No medium (in tape drive)",
                156: "Level 2 not synchronized"
            };
            var ERRNO_CODES = {};
            var FS = {
                root: null,
                mounts: [],
                devices: {},
                streams: [],
                nextInode: 1,
                nameTable: null,
                currentPath: "/",
                initialized: false,
                ignorePermissions: true,
                ErrnoError: null,
                genericErrors: {},
                filesystems: null,
                syncFSRequests: 0,
                lookupPath: (path, opts = {}) => {
                    path = PATH_FS.resolve(FS.cwd(), path);
                    if (!path) return {
                        path: "",
                        node: null
                    };
                    var defaults = {
                        follow_mount: true,
                        recurse_count: 0
                    };
                    for (var key in defaults) {
                        if (opts[key] === undefined) {
                            opts[key] = defaults[key]
                        }
                    }
                    if (opts.recurse_count > 8) {
                        throw new FS.ErrnoError(32)
                    }
                    var parts = PATH.normalizeArray(path.split("/").filter(p => !!p), false);
                    var current = FS.root;
                    var current_path = "/";
                    for (var i = 0; i < parts.length; i++) {
                        var islast = i === parts.length - 1;
                        if (islast && opts.parent) {
                            break
                        }
                        current = FS.lookupNode(current, parts[i]);
                        current_path = PATH.join2(current_path, parts[i]);
                        if (FS.isMountpoint(current)) {
                            if (!islast || islast && opts.follow_mount) {
                                current = current.mounted.root
                            }
                        }
                        if (!islast || opts.follow) {
                            var count = 0;
                            while (FS.isLink(current.mode)) {
                                var link = FS.readlink(current_path);
                                current_path = PATH_FS.resolve(PATH.dirname(current_path), link);
                                var lookup = FS.lookupPath(current_path, {
                                    recurse_count: opts.recurse_count
                                });
                                current = lookup.node;
                                if (count++ > 40) {
                                    throw new FS.ErrnoError(32)
                                }
                            }
                        }
                    }
                    return {
                        path: current_path,
                        node: current
                    }
                },
                getPath: node => {
                    var path;
                    while (true) {
                        if (FS.isRoot(node)) {
                            var mount = node.mount.mountpoint;
                            if (!path) return mount;
                            return mount[mount.length - 1] !== "/" ? mount + "/" + path : mount + path
                        }
                        path = path ? node.name + "/" + path : node.name;
                        node = node.parent
                    }
                },
                hashName: (parentid, name) => {
                    var hash = 0;
                    for (var i = 0; i < name.length; i++) {
                        hash = (hash << 5) - hash + name.charCodeAt(i) | 0
                    }
                    return (parentid + hash >>> 0) % FS.nameTable.length
                },
                hashAddNode: node => {
                    var hash = FS.hashName(node.parent.id, node.name);
                    node.name_next = FS.nameTable[hash];
                    FS.nameTable[hash] = node
                },
                hashRemoveNode: node => {
                    var hash = FS.hashName(node.parent.id, node.name);
                    if (FS.nameTable[hash] === node) {
                        FS.nameTable[hash] = node.name_next
                    } else {
                        var current = FS.nameTable[hash];
                        while (current) {
                            if (current.name_next === node) {
                                current.name_next = node.name_next;
                                break
                            }
                            current = current.name_next
                        }
                    }
                },
                lookupNode: (parent, name) => {
                    var errCode = FS.mayLookup(parent);
                    if (errCode) {
                        throw new FS.ErrnoError(errCode, parent)
                    }
                    var hash = FS.hashName(parent.id, name);
                    for (var node = FS.nameTable[hash]; node; node = node.name_next) {
                        var nodeName = node.name;
                        if (node.parent.id === parent.id && nodeName === name) {
                            return node
                        }
                    }
                    return FS.lookup(parent, name)
                },
                createNode: (parent, name, mode, rdev) => {
                    assert(typeof parent === "object");
                    var node = new FS.FSNode(parent, name, mode, rdev);
                    FS.hashAddNode(node);
                    return node
                },
                destroyNode: node => {
                    FS.hashRemoveNode(node)
                },
                isRoot: node => {
                    return node === node.parent
                },
                isMountpoint: node => {
                    return !!node.mounted
                },
                isFile: mode => {
                    return (mode & 61440) === 32768
                },
                isDir: mode => {
                    return (mode & 61440) === 16384
                },
                isLink: mode => {
                    return (mode & 61440) === 40960
                },
                isChrdev: mode => {
                    return (mode & 61440) === 8192
                },
                isBlkdev: mode => {
                    return (mode & 61440) === 24576
                },
                isFIFO: mode => {
                    return (mode & 61440) === 4096
                },
                isSocket: mode => {
                    return (mode & 49152) === 49152
                },
                flagModes: {
                    "r": 0,
                    "r+": 2,
                    "w": 577,
                    "w+": 578,
                    "a": 1089,
                    "a+": 1090
                },
                modeStringToFlags: str => {
                    var flags = FS.flagModes[str];
                    if (typeof flags === "undefined") {
                        throw new Error("Unknown file open mode: " + str)
                    }
                    return flags
                },
                flagsToPermissionString: flag => {
                    var perms = ["r", "w", "rw"][flag & 3];
                    if (flag & 512) {
                        perms += "w"
                    }
                    return perms
                },
                nodePermissions: (node, perms) => {
                    if (FS.ignorePermissions) {
                        return 0
                    }
                    if (perms.includes("r") && !(node.mode & 292)) {
                        return 2
                    } else if (perms.includes("w") && !(node.mode & 146)) {
                        return 2
                    } else if (perms.includes("x") && !(node.mode & 73)) {
                        return 2
                    }
                    return 0
                },
                mayLookup: dir => {
                    var errCode = FS.nodePermissions(dir, "x");
                    if (errCode) return errCode;
                    if (!dir.node_ops.lookup) return 2;
                    return 0
                },
                mayCreate: (dir, name) => {
                    try {
                        var node = FS.lookupNode(dir, name);
                        return 20
                    } catch (e) {}
                    return FS.nodePermissions(dir, "wx")
                },
                mayDelete: (dir, name, isdir) => {
                    var node;
                    try {
                        node = FS.lookupNode(dir, name)
                    } catch (e) {
                        return e.errno
                    }
                    var errCode = FS.nodePermissions(dir, "wx");
                    if (errCode) {
                        return errCode
                    }
                    if (isdir) {
                        if (!FS.isDir(node.mode)) {
                            return 54
                        }
                        if (FS.isRoot(node) || FS.getPath(node) === FS.cwd()) {
                            return 10
                        }
                    } else {
                        if (FS.isDir(node.mode)) {
                            return 31
                        }
                    }
                    return 0
                },
                mayOpen: (node, flags) => {
                    if (!node) {
                        return 44
                    }
                    if (FS.isLink(node.mode)) {
                        return 32
                    } else if (FS.isDir(node.mode)) {
                        if (FS.flagsToPermissionString(flags) !== "r" || flags & 512) {
                            return 31
                        }
                    }
                    return FS.nodePermissions(node, FS.flagsToPermissionString(flags))
                },
                MAX_OPEN_FDS: 4096,
                nextfd: (fd_start = 0, fd_end = FS.MAX_OPEN_FDS) => {
                    for (var fd = fd_start; fd <= fd_end; fd++) {
                        if (!FS.streams[fd]) {
                            return fd
                        }
                    }
                    throw new FS.ErrnoError(33)
                },
                getStream: fd => FS.streams[fd],
                createStream: (stream, fd_start, fd_end) => {
                    if (!FS.FSStream) {
                        FS.FSStream = function() {};
                        FS.FSStream.prototype = {
                            object: {
                                get: function() {
                                    return this.node
                                },
                                set: function(val) {
                                    this.node = val
                                }
                            },
                            isRead: {
                                get: function() {
                                    return (this.flags & 2097155) !== 1
                                }
                            },
                            isWrite: {
                                get: function() {
                                    return (this.flags & 2097155) !== 0
                                }
                            },
                            isAppend: {
                                get: function() {
                                    return this.flags & 1024
                                }
                            }
                        }
                    }
                    stream = Object.assign(new FS.FSStream, stream);
                    var fd = FS.nextfd(fd_start, fd_end);
                    stream.fd = fd;
                    FS.streams[fd] = stream;
                    return stream
                },
                closeStream: fd => {
                    FS.streams[fd] = null
                },
                chrdev_stream_ops: {
                    open: stream => {
                        var device = FS.getDevice(stream.node.rdev);
                        stream.stream_ops = device.stream_ops;
                        if (stream.stream_ops.open) {
                            stream.stream_ops.open(stream)
                        }
                    },
                    llseek: () => {
                        throw new FS.ErrnoError(70)
                    }
                },
                major: dev => dev >> 8,
                minor: dev => dev & 255,
                makedev: (ma, mi) => ma << 8 | mi,
                registerDevice: (dev, ops) => {
                    FS.devices[dev] = {
                        stream_ops: ops
                    }
                },
                getDevice: dev => FS.devices[dev],
                getMounts: mount => {
                    var mounts = [];
                    var check = [mount];
                    while (check.length) {
                        var m = check.pop();
                        mounts.push(m);
                        check.push.apply(check, m.mounts)
                    }
                    return mounts
                },
                syncfs: (populate, callback) => {
                    if (typeof populate === "function") {
                        callback = populate;
                        populate = false
                    }
                    FS.syncFSRequests++;
                    if (FS.syncFSRequests > 1) {
                        err("warning: " + FS.syncFSRequests + " FS.syncfs operations in flight at once, probably just doing extra work")
                    }
                    var mounts = FS.getMounts(FS.root.mount);
                    var completed = 0;

                    function doCallback(errCode) {
                        assert(FS.syncFSRequests > 0);
                        FS.syncFSRequests--;
                        return callback(errCode)
                    }

                    function done(errCode) {
                        if (errCode) {
                            if (!done.errored) {
                                done.errored = true;
                                return doCallback(errCode)
                            }
                            return
                        }
                        if (++completed >= mounts.length) {
                            doCallback(null)
                        }
                    }
                    mounts.forEach(mount => {
                        if (!mount.type.syncfs) {
                            return done(null)
                        }
                        mount.type.syncfs(mount, populate, done)
                    })
                },
                mount: (type, opts, mountpoint) => {
                    if (typeof type === "string") {
                        throw type
                    }
                    var root = mountpoint === "/";
                    var pseudo = !mountpoint;
                    var node;
                    if (root && FS.root) {
                        throw new FS.ErrnoError(10)
                    } else if (!root && !pseudo) {
                        var lookup = FS.lookupPath(mountpoint, {
                            follow_mount: false
                        });
                        mountpoint = lookup.path;
                        node = lookup.node;
                        if (FS.isMountpoint(node)) {
                            throw new FS.ErrnoError(10)
                        }
                        if (!FS.isDir(node.mode)) {
                            throw new FS.ErrnoError(54)
                        }
                    }
                    var mount = {
                        type: type,
                        opts: opts,
                        mountpoint: mountpoint,
                        mounts: []
                    };
                    var mountRoot = type.mount(mount);
                    mountRoot.mount = mount;
                    mount.root = mountRoot;
                    if (root) {
                        FS.root = mountRoot
                    } else if (node) {
                        node.mounted = mount;
                        if (node.mount) {
                            node.mount.mounts.push(mount)
                        }
                    }
                    return mountRoot
                },
                unmount: mountpoint => {
                    var lookup = FS.lookupPath(mountpoint, {
                        follow_mount: false
                    });
                    if (!FS.isMountpoint(lookup.node)) {
                        throw new FS.ErrnoError(28)
                    }
                    var node = lookup.node;
                    var mount = node.mounted;
                    var mounts = FS.getMounts(mount);
                    Object.keys(FS.nameTable).forEach(hash => {
                        var current = FS.nameTable[hash];
                        while (current) {
                            var next = current.name_next;
                            if (mounts.includes(current.mount)) {
                                FS.destroyNode(current)
                            }
                            current = next
                        }
                    });
                    node.mounted = null;
                    var idx = node.mount.mounts.indexOf(mount);
                    assert(idx !== -1);
                    node.mount.mounts.splice(idx, 1)
                },
                lookup: (parent, name) => {
                    return parent.node_ops.lookup(parent, name)
                },
                mknod: (path, mode, dev) => {
                    var lookup = FS.lookupPath(path, {
                        parent: true
                    });
                    var parent = lookup.node;
                    var name = PATH.basename(path);
                    if (!name || name === "." || name === "..") {
                        throw new FS.ErrnoError(28)
                    }
                    var errCode = FS.mayCreate(parent, name);
                    if (errCode) {
                        throw new FS.ErrnoError(errCode)
                    }
                    if (!parent.node_ops.mknod) {
                        throw new FS.ErrnoError(63)
                    }
                    return parent.node_ops.mknod(parent, name, mode, dev)
                },
                create: (path, mode) => {
                    mode = mode !== undefined ? mode : 438;
                    mode &= 4095;
                    mode |= 32768;
                    return FS.mknod(path, mode, 0)
                },
                mkdir: (path, mode) => {
                    mode = mode !== undefined ? mode : 511;
                    mode &= 511 | 512;
                    mode |= 16384;
                    return FS.mknod(path, mode, 0)
                },
                mkdirTree: (path, mode) => {
                    var dirs = path.split("/");
                    var d = "";
                    for (var i = 0; i < dirs.length; ++i) {
                        if (!dirs[i]) continue;
                        d += "/" + dirs[i];
                        try {
                            FS.mkdir(d, mode)
                        } catch (e) {
                            if (e.errno != 20) throw e
                        }
                    }
                },
                mkdev: (path, mode, dev) => {
                    if (typeof dev === "undefined") {
                        dev = mode;
                        mode = 438
                    }
                    mode |= 8192;
                    return FS.mknod(path, mode, dev)
                },
                symlink: (oldpath, newpath) => {
                    if (!PATH_FS.resolve(oldpath)) {
                        throw new FS.ErrnoError(44)
                    }
                    var lookup = FS.lookupPath(newpath, {
                        parent: true
                    });
                    var parent = lookup.node;
                    if (!parent) {
                        throw new FS.ErrnoError(44)
                    }
                    var newname = PATH.basename(newpath);
                    var errCode = FS.mayCreate(parent, newname);
                    if (errCode) {
                        throw new FS.ErrnoError(errCode)
                    }
                    if (!parent.node_ops.symlink) {
                        throw new FS.ErrnoError(63)
                    }
                    return parent.node_ops.symlink(parent, newname, oldpath)
                },
                rename: (old_path, new_path) => {
                    var old_dirname = PATH.dirname(old_path);
                    var new_dirname = PATH.dirname(new_path);
                    var old_name = PATH.basename(old_path);
                    var new_name = PATH.basename(new_path);
                    var lookup, old_dir, new_dir;
                    lookup = FS.lookupPath(old_path, {
                        parent: true
                    });
                    old_dir = lookup.node;
                    lookup = FS.lookupPath(new_path, {
                        parent: true
                    });
                    new_dir = lookup.node;
                    if (!old_dir || !new_dir) throw new FS.ErrnoError(44);
                    if (old_dir.mount !== new_dir.mount) {
                        throw new FS.ErrnoError(75)
                    }
                    var old_node = FS.lookupNode(old_dir, old_name);
                    var relative = PATH_FS.relative(old_path, new_dirname);
                    if (relative.charAt(0) !== ".") {
                        throw new FS.ErrnoError(28)
                    }
                    relative = PATH_FS.relative(new_path, old_dirname);
                    if (relative.charAt(0) !== ".") {
                        throw new FS.ErrnoError(55)
                    }
                    var new_node;
                    try {
                        new_node = FS.lookupNode(new_dir, new_name)
                    } catch (e) {}
                    if (old_node === new_node) {
                        return
                    }
                    var isdir = FS.isDir(old_node.mode);
                    var errCode = FS.mayDelete(old_dir, old_name, isdir);
                    if (errCode) {
                        throw new FS.ErrnoError(errCode)
                    }
                    errCode = new_node ? FS.mayDelete(new_dir, new_name, isdir) : FS.mayCreate(new_dir, new_name);
                    if (errCode) {
                        throw new FS.ErrnoError(errCode)
                    }
                    if (!old_dir.node_ops.rename) {
                        throw new FS.ErrnoError(63)
                    }
                    if (FS.isMountpoint(old_node) || new_node && FS.isMountpoint(new_node)) {
                        throw new FS.ErrnoError(10)
                    }
                    if (new_dir !== old_dir) {
                        errCode = FS.nodePermissions(old_dir, "w");
                        if (errCode) {
                            throw new FS.ErrnoError(errCode)
                        }
                    }
                    FS.hashRemoveNode(old_node);
                    try {
                        old_dir.node_ops.rename(old_node, new_dir, new_name)
                    } catch (e) {
                        throw e
                    } finally {
                        FS.hashAddNode(old_node)
                    }
                },
                rmdir: path => {
                    var lookup = FS.lookupPath(path, {
                        parent: true
                    });
                    var parent = lookup.node;
                    var name = PATH.basename(path);
                    var node = FS.lookupNode(parent, name);
                    var errCode = FS.mayDelete(parent, name, true);
                    if (errCode) {
                        throw new FS.ErrnoError(errCode)
                    }
                    if (!parent.node_ops.rmdir) {
                        throw new FS.ErrnoError(63)
                    }
                    if (FS.isMountpoint(node)) {
                        throw new FS.ErrnoError(10)
                    }
                    parent.node_ops.rmdir(parent, name);
                    FS.destroyNode(node)
                },
                readdir: path => {
                    var lookup = FS.lookupPath(path, {
                        follow: true
                    });
                    var node = lookup.node;
                    if (!node.node_ops.readdir) {
                        throw new FS.ErrnoError(54)
                    }
                    return node.node_ops.readdir(node)
                },
                unlink: path => {
                    var lookup = FS.lookupPath(path, {
                        parent: true
                    });
                    var parent = lookup.node;
                    if (!parent) {
                        throw new FS.ErrnoError(44)
                    }
                    var name = PATH.basename(path);
                    var node = FS.lookupNode(parent, name);
                    var errCode = FS.mayDelete(parent, name, false);
                    if (errCode) {
                        throw new FS.ErrnoError(errCode)
                    }
                    if (!parent.node_ops.unlink) {
                        throw new FS.ErrnoError(63)
                    }
                    if (FS.isMountpoint(node)) {
                        throw new FS.ErrnoError(10)
                    }
                    parent.node_ops.unlink(parent, name);
                    FS.destroyNode(node)
                },
                readlink: path => {
                    var lookup = FS.lookupPath(path);
                    var link = lookup.node;
                    if (!link) {
                        throw new FS.ErrnoError(44)
                    }
                    if (!link.node_ops.readlink) {
                        throw new FS.ErrnoError(28)
                    }
                    return PATH_FS.resolve(FS.getPath(link.parent), link.node_ops.readlink(link))
                },
                stat: (path, dontFollow) => {
                    var lookup = FS.lookupPath(path, {
                        follow: !dontFollow
                    });
                    var node = lookup.node;
                    if (!node) {
                        throw new FS.ErrnoError(44)
                    }
                    if (!node.node_ops.getattr) {
                        throw new FS.ErrnoError(63)
                    }
                    return node.node_ops.getattr(node)
                },
                lstat: path => {
                    return FS.stat(path, true)
                },
                chmod: (path, mode, dontFollow) => {
                    var node;
                    if (typeof path === "string") {
                        var lookup = FS.lookupPath(path, {
                            follow: !dontFollow
                        });
                        node = lookup.node
                    } else {
                        node = path
                    }
                    if (!node.node_ops.setattr) {
                        throw new FS.ErrnoError(63)
                    }
                    node.node_ops.setattr(node, {
                        mode: mode & 4095 | node.mode & ~4095,
                        timestamp: Date.now()
                    })
                },
                lchmod: (path, mode) => {
                    FS.chmod(path, mode, true)
                },
                fchmod: (fd, mode) => {
                    var stream = FS.getStream(fd);
                    if (!stream) {
                        throw new FS.ErrnoError(8)
                    }
                    FS.chmod(stream.node, mode)
                },
                chown: (path, uid, gid, dontFollow) => {
                    var node;
                    if (typeof path === "string") {
                        var lookup = FS.lookupPath(path, {
                            follow: !dontFollow
                        });
                        node = lookup.node
                    } else {
                        node = path
                    }
                    if (!node.node_ops.setattr) {
                        throw new FS.ErrnoError(63)
                    }
                    node.node_ops.setattr(node, {
                        timestamp: Date.now()
                    })
                },
                lchown: (path, uid, gid) => {
                    FS.chown(path, uid, gid, true)
                },
                fchown: (fd, uid, gid) => {
                    var stream = FS.getStream(fd);
                    if (!stream) {
                        throw new FS.ErrnoError(8)
                    }
                    FS.chown(stream.node, uid, gid)
                },
                truncate: (path, len) => {
                    if (len < 0) {
                        throw new FS.ErrnoError(28)
                    }
                    var node;
                    if (typeof path === "string") {
                        var lookup = FS.lookupPath(path, {
                            follow: true
                        });
                        node = lookup.node
                    } else {
                        node = path
                    }
                    if (!node.node_ops.setattr) {
                        throw new FS.ErrnoError(63)
                    }
                    if (FS.isDir(node.mode)) {
                        throw new FS.ErrnoError(31)
                    }
                    if (!FS.isFile(node.mode)) {
                        throw new FS.ErrnoError(28)
                    }
                    var errCode = FS.nodePermissions(node, "w");
                    if (errCode) {
                        throw new FS.ErrnoError(errCode)
                    }
                    node.node_ops.setattr(node, {
                        size: len,
                        timestamp: Date.now()
                    })
                },
                ftruncate: (fd, len) => {
                    var stream = FS.getStream(fd);
                    if (!stream) {
                        throw new FS.ErrnoError(8)
                    }
                    if ((stream.flags & 2097155) === 0) {
                        throw new FS.ErrnoError(28)
                    }
                    FS.truncate(stream.node, len)
                },
                utime: (path, atime, mtime) => {
                    var lookup = FS.lookupPath(path, {
                        follow: true
                    });
                    var node = lookup.node;
                    node.node_ops.setattr(node, {
                        timestamp: Math.max(atime, mtime)
                    })
                },
                open: (path, flags, mode, fd_start, fd_end) => {
                    if (path === "") {
                        throw new FS.ErrnoError(44)
                    }
                    flags = typeof flags === "string" ? FS.modeStringToFlags(flags) : flags;
                    mode = typeof mode === "undefined" ? 438 : mode;
                    if (flags & 64) {
                        mode = mode & 4095 | 32768
                    } else {
                        mode = 0
                    }
                    var node;
                    if (typeof path === "object") {
                        node = path
                    } else {
                        path = PATH.normalize(path);
                        try {
                            var lookup = FS.lookupPath(path, {
                                follow: !(flags & 131072)
                            });
                            node = lookup.node
                        } catch (e) {}
                    }
                    var created = false;
                    if (flags & 64) {
                        if (node) {
                            if (flags & 128) {
                                throw new FS.ErrnoError(20)
                            }
                        } else {
                            node = FS.mknod(path, mode, 0);
                            created = true
                        }
                    }
                    if (!node) {
                        throw new FS.ErrnoError(44)
                    }
                    if (FS.isChrdev(node.mode)) {
                        flags &= ~512
                    }
                    if (flags & 65536 && !FS.isDir(node.mode)) {
                        throw new FS.ErrnoError(54)
                    }
                    if (!created) {
                        var errCode = FS.mayOpen(node, flags);
                        if (errCode) {
                            throw new FS.ErrnoError(errCode)
                        }
                    }
                    if (flags & 512) {
                        FS.truncate(node, 0)
                    }
                    flags &= ~(128 | 512 | 131072);
                    var stream = FS.createStream({
                        node: node,
                        path: FS.getPath(node),
                        flags: flags,
                        seekable: true,
                        position: 0,
                        stream_ops: node.stream_ops,
                        ungotten: [],
                        error: false
                    }, fd_start, fd_end);
                    if (stream.stream_ops.open) {
                        stream.stream_ops.open(stream)
                    }
                    if (Module["logReadFiles"] && !(flags & 1)) {
                        if (!FS.readFiles) FS.readFiles = {};
                        if (!(path in FS.readFiles)) {
                            FS.readFiles[path] = 1
                        }
                    }
                    return stream
                },
                close: stream => {
                    if (FS.isClosed(stream)) {
                        throw new FS.ErrnoError(8)
                    }
                    if (stream.getdents) stream.getdents = null;
                    try {
                        if (stream.stream_ops.close) {
                            stream.stream_ops.close(stream)
                        }
                    } catch (e) {
                        throw e
                    } finally {
                        FS.closeStream(stream.fd)
                    }
                    stream.fd = null
                },
                isClosed: stream => {
                    return stream.fd === null
                },
                llseek: (stream, offset, whence) => {
                    if (FS.isClosed(stream)) {
                        throw new FS.ErrnoError(8)
                    }
                    if (!stream.seekable || !stream.stream_ops.llseek) {
                        throw new FS.ErrnoError(70)
                    }
                    if (whence != 0 && whence != 1 && whence != 2) {
                        throw new FS.ErrnoError(28)
                    }
                    stream.position = stream.stream_ops.llseek(stream, offset, whence);
                    stream.ungotten = [];
                    return stream.position
                },
                read: (stream, buffer, offset, length, position) => {
                    if (length < 0 || position < 0) {
                        throw new FS.ErrnoError(28)
                    }
                    if (FS.isClosed(stream)) {
                        throw new FS.ErrnoError(8)
                    }
                    if ((stream.flags & 2097155) === 1) {
                        throw new FS.ErrnoError(8)
                    }
                    if (FS.isDir(stream.node.mode)) {
                        throw new FS.ErrnoError(31)
                    }
                    if (!stream.stream_ops.read) {
                        throw new FS.ErrnoError(28)
                    }
                    var seeking = typeof position !== "undefined";
                    if (!seeking) {
                        position = stream.position
                    } else if (!stream.seekable) {
                        throw new FS.ErrnoError(70)
                    }
                    var bytesRead = stream.stream_ops.read(stream, buffer, offset, length, position);
                    if (!seeking) stream.position += bytesRead;
                    return bytesRead
                },
                write: (stream, buffer, offset, length, position, canOwn) => {
                    if (length < 0 || position < 0) {
                        throw new FS.ErrnoError(28)
                    }
                    if (FS.isClosed(stream)) {
                        throw new FS.ErrnoError(8)
                    }
                    if ((stream.flags & 2097155) === 0) {
                        throw new FS.ErrnoError(8)
                    }
                    if (FS.isDir(stream.node.mode)) {
                        throw new FS.ErrnoError(31)
                    }
                    if (!stream.stream_ops.write) {
                        throw new FS.ErrnoError(28)
                    }
                    if (stream.seekable && stream.flags & 1024) {
                        FS.llseek(stream, 0, 2)
                    }
                    var seeking = typeof position !== "undefined";
                    if (!seeking) {
                        position = stream.position
                    } else if (!stream.seekable) {
                        throw new FS.ErrnoError(70)
                    }
                    var bytesWritten = stream.stream_ops.write(stream, buffer, offset, length, position, canOwn);
                    if (!seeking) stream.position += bytesWritten;
                    return bytesWritten
                },
                allocate: (stream, offset, length) => {
                    if (FS.isClosed(stream)) {
                        throw new FS.ErrnoError(8)
                    }
                    if (offset < 0 || length <= 0) {
                        throw new FS.ErrnoError(28)
                    }
                    if ((stream.flags & 2097155) === 0) {
                        throw new FS.ErrnoError(8)
                    }
                    if (!FS.isFile(stream.node.mode) && !FS.isDir(stream.node.mode)) {
                        throw new FS.ErrnoError(43)
                    }
                    if (!stream.stream_ops.allocate) {
                        throw new FS.ErrnoError(138)
                    }
                    stream.stream_ops.allocate(stream, offset, length)
                },
                mmap: (stream, address, length, position, prot, flags) => {
                    if ((prot & 2) !== 0 && (flags & 2) === 0 && (stream.flags & 2097155) !== 2) {
                        throw new FS.ErrnoError(2)
                    }
                    if ((stream.flags & 2097155) === 1) {
                        throw new FS.ErrnoError(2)
                    }
                    if (!stream.stream_ops.mmap) {
                        throw new FS.ErrnoError(43)
                    }
                    return stream.stream_ops.mmap(stream, address, length, position, prot, flags)
                },
                msync: (stream, buffer, offset, length, mmapFlags) => {
                    if (!stream || !stream.stream_ops.msync) {
                        return 0
                    }
                    return stream.stream_ops.msync(stream, buffer, offset, length, mmapFlags)
                },
                munmap: stream => 0,
                ioctl: (stream, cmd, arg) => {
                    if (!stream.stream_ops.ioctl) {
                        throw new FS.ErrnoError(59)
                    }
                    return stream.stream_ops.ioctl(stream, cmd, arg)
                },
                readFile: (path, opts = {}) => {
                    opts.flags = opts.flags || 0;
                    opts.encoding = opts.encoding || "binary";
                    if (opts.encoding !== "utf8" && opts.encoding !== "binary") {
                        throw new Error('Invalid encoding type "' + opts.encoding + '"')
                    }
                    var ret;
                    var stream = FS.open(path, opts.flags);
                    var stat = FS.stat(path);
                    var length = stat.size;
                    var buf = new Uint8Array(length);
                    FS.read(stream, buf, 0, length, 0);
                    if (opts.encoding === "utf8") {
                        ret = UTF8ArrayToString(buf, 0)
                    } else if (opts.encoding === "binary") {
                        ret = buf
                    }
                    FS.close(stream);
                    return ret
                },
                writeFile: (path, data, opts = {}) => {
                    opts.flags = opts.flags || 577;
                    var stream = FS.open(path, opts.flags, opts.mode);
                    if (typeof data === "string") {
                        var buf = new Uint8Array(lengthBytesUTF8(data) + 1);
                        var actualNumBytes = stringToUTF8Array(data, buf, 0, buf.length);
                        FS.write(stream, buf, 0, actualNumBytes, undefined, opts.canOwn)
                    } else if (ArrayBuffer.isView(data)) {
                        FS.write(stream, data, 0, data.byteLength, undefined, opts.canOwn)
                    } else {
                        throw new Error("Unsupported data type")
                    }
                    FS.close(stream)
                },
                cwd: () => FS.currentPath,
                chdir: path => {
                    var lookup = FS.lookupPath(path, {
                        follow: true
                    });
                    if (lookup.node === null) {
                        throw new FS.ErrnoError(44)
                    }
                    if (!FS.isDir(lookup.node.mode)) {
                        throw new FS.ErrnoError(54)
                    }
                    var errCode = FS.nodePermissions(lookup.node, "x");
                    if (errCode) {
                        throw new FS.ErrnoError(errCode)
                    }
                    FS.currentPath = lookup.path
                },
                createDefaultDirectories: () => {
                    FS.mkdir("/tmp");
                    FS.mkdir("/home");
                    FS.mkdir("/home/web_user")
                },
                createDefaultDevices: () => {
                    FS.mkdir("/dev");
                    FS.registerDevice(FS.makedev(1, 3), {
                        read: () => 0,
                        write: (stream, buffer, offset, length, pos) => length
                    });
                    FS.mkdev("/dev/null", FS.makedev(1, 3));
                    TTY.register(FS.makedev(5, 0), TTY.default_tty_ops);
                    TTY.register(FS.makedev(6, 0), TTY.default_tty1_ops);
                    FS.mkdev("/dev/tty", FS.makedev(5, 0));
                    FS.mkdev("/dev/tty1", FS.makedev(6, 0));
                    var random_device = getRandomDevice();
                    FS.createDevice("/dev", "random", random_device);
                    FS.createDevice("/dev", "urandom", random_device);
                    FS.mkdir("/dev/shm");
                    FS.mkdir("/dev/shm/tmp")
                },
                createSpecialDirectories: () => {
                    FS.mkdir("/proc");
                    var proc_self = FS.mkdir("/proc/self");
                    FS.mkdir("/proc/self/fd");
                    FS.mount({
                        mount: () => {
                            var node = FS.createNode(proc_self, "fd", 16384 | 511, 73);
                            node.node_ops = {
                                lookup: (parent, name) => {
                                    var fd = +name;
                                    var stream = FS.getStream(fd);
                                    if (!stream) throw new FS.ErrnoError(8);
                                    var ret = {
                                        parent: null,
                                        mount: {
                                            mountpoint: "fake"
                                        },
                                        node_ops: {
                                            readlink: () => stream.path
                                        }
                                    };
                                    ret.parent = ret;
                                    return ret
                                }
                            };
                            return node
                        }
                    }, {}, "/proc/self/fd")
                },
                createStandardStreams: () => {
                    if (Module["stdin"]) {
                        FS.createDevice("/dev", "stdin", Module["stdin"])
                    } else {
                        FS.symlink("/dev/tty", "/dev/stdin")
                    }
                    if (Module["stdout"]) {
                        FS.createDevice("/dev", "stdout", null, Module["stdout"])
                    } else {
                        FS.symlink("/dev/tty", "/dev/stdout")
                    }
                    if (Module["stderr"]) {
                        FS.createDevice("/dev", "stderr", null, Module["stderr"])
                    } else {
                        FS.symlink("/dev/tty1", "/dev/stderr")
                    }
                    var stdin = FS.open("/dev/stdin", 0);
                    var stdout = FS.open("/dev/stdout", 1);
                    var stderr = FS.open("/dev/stderr", 1);
                    assert(stdin.fd === 0, "invalid handle for stdin (" + stdin.fd + ")");
                    assert(stdout.fd === 1, "invalid handle for stdout (" + stdout.fd + ")");
                    assert(stderr.fd === 2, "invalid handle for stderr (" + stderr.fd + ")")
                },
                ensureErrnoError: () => {
                    if (FS.ErrnoError) return;
                    FS.ErrnoError = function ErrnoError(errno, node) {
                        this.node = node;
                        this.setErrno = function(errno) {
                            this.errno = errno;
                            for (var key in ERRNO_CODES) {
                                if (ERRNO_CODES[key] === errno) {
                                    this.code = key;
                                    break
                                }
                            }
                        };
                        this.setErrno(errno);
                        this.message = ERRNO_MESSAGES[errno];
                        if (this.stack) {
                            Object.defineProperty(this, "stack", {
                                value: (new Error).stack,
                                writable: true
                            });
                            this.stack = demangleAll(this.stack)
                        }
                    };
                    FS.ErrnoError.prototype = new Error;
                    FS.ErrnoError.prototype.constructor = FS.ErrnoError;
                    [44].forEach(code => {
                        FS.genericErrors[code] = new FS.ErrnoError(code);
                        FS.genericErrors[code].stack = "<generic error, no stack>"
                    })
                },
                staticInit: () => {
                    FS.ensureErrnoError();
                    FS.nameTable = new Array(4096);
                    FS.mount(MEMFS, {}, "/");
                    FS.createDefaultDirectories();
                    FS.createDefaultDevices();
                    FS.createSpecialDirectories();
                    FS.filesystems = {
                        "MEMFS": MEMFS
                    }
                },
                init: (input, output, error) => {
                    assert(!FS.init.initialized, "FS.init was previously called. If you want to initialize later with custom parameters, remove any earlier calls (note that one is automatically added to the generated code)");
                    FS.init.initialized = true;
                    FS.ensureErrnoError();
                    Module["stdin"] = input || Module["stdin"];
                    Module["stdout"] = output || Module["stdout"];
                    Module["stderr"] = error || Module["stderr"];
                    FS.createStandardStreams()
                },
                quit: () => {
                    FS.init.initialized = false;
                    ___stdio_exit();
                    for (var i = 0; i < FS.streams.length; i++) {
                        var stream = FS.streams[i];
                        if (!stream) {
                            continue
                        }
                        FS.close(stream)
                    }
                },
                getMode: (canRead, canWrite) => {
                    var mode = 0;
                    if (canRead) mode |= 292 | 73;
                    if (canWrite) mode |= 146;
                    return mode
                },
                findObject: (path, dontResolveLastLink) => {
                    var ret = FS.analyzePath(path, dontResolveLastLink);
                    if (ret.exists) {
                        return ret.object
                    } else {
                        return null
                    }
                },
                analyzePath: (path, dontResolveLastLink) => {
                    try {
                        var lookup = FS.lookupPath(path, {
                            follow: !dontResolveLastLink
                        });
                        path = lookup.path
                    } catch (e) {}
                    var ret = {
                        isRoot: false,
                        exists: false,
                        error: 0,
                        name: null,
                        path: null,
                        object: null,
                        parentExists: false,
                        parentPath: null,
                        parentObject: null
                    };
                    try {
                        var lookup = FS.lookupPath(path, {
                            parent: true
                        });
                        ret.parentExists = true;
                        ret.parentPath = lookup.path;
                        ret.parentObject = lookup.node;
                        ret.name = PATH.basename(path);
                        lookup = FS.lookupPath(path, {
                            follow: !dontResolveLastLink
                        });
                        ret.exists = true;
                        ret.path = lookup.path;
                        ret.object = lookup.node;
                        ret.name = lookup.node.name;
                        ret.isRoot = lookup.path === "/"
                    } catch (e) {
                        ret.error = e.errno
                    }
                    return ret
                },
                createPath: (parent, path, canRead, canWrite) => {
                    parent = typeof parent === "string" ? parent : FS.getPath(parent);
                    var parts = path.split("/").reverse();
                    while (parts.length) {
                        var part = parts.pop();
                        if (!part) continue;
                        var current = PATH.join2(parent, part);
                        try {
                            FS.mkdir(current)
                        } catch (e) {}
                        parent = current
                    }
                    return current
                },
                createFile: (parent, name, properties, canRead, canWrite) => {
                    var path = PATH.join2(typeof parent === "string" ? parent : FS.getPath(parent), name);
                    var mode = FS.getMode(canRead, canWrite);
                    return FS.create(path, mode)
                },
                createDataFile: (parent, name, data, canRead, canWrite, canOwn) => {
                    var path = name;
                    if (parent) {
                        parent = typeof parent === "string" ? parent : FS.getPath(parent);
                        path = name ? PATH.join2(parent, name) : parent
                    }
                    var mode = FS.getMode(canRead, canWrite);
                    var node = FS.create(path, mode);
                    if (data) {
                        if (typeof data === "string") {
                            var arr = new Array(data.length);
                            for (var i = 0, len = data.length; i < len; ++i) arr[i] = data.charCodeAt(i);
                            data = arr
                        }
                        FS.chmod(node, mode | 146);
                        var stream = FS.open(node, 577);
                        FS.write(stream, data, 0, data.length, 0, canOwn);
                        FS.close(stream);
                        FS.chmod(node, mode)
                    }
                    return node
                },
                createDevice: (parent, name, input, output) => {
                    var path = PATH.join2(typeof parent === "string" ? parent : FS.getPath(parent), name);
                    var mode = FS.getMode(!!input, !!output);
                    if (!FS.createDevice.major) FS.createDevice.major = 64;
                    var dev = FS.makedev(FS.createDevice.major++, 0);
                    FS.registerDevice(dev, {
                        open: stream => {
                            stream.seekable = false
                        },
                        close: stream => {
                            if (output && output.buffer && output.buffer.length) {
                                output(10)
                            }
                        },
                        read: (stream, buffer, offset, length, pos) => {
                            var bytesRead = 0;
                            for (var i = 0; i < length; i++) {
                                var result;
                                try {
                                    result = input()
                                } catch (e) {
                                    throw new FS.ErrnoError(29)
                                }
                                if (result === undefined && bytesRead === 0) {
                                    throw new FS.ErrnoError(6)
                                }
                                if (result === null || result === undefined) break;
                                bytesRead++;
                                buffer[offset + i] = result
                            }
                            if (bytesRead) {
                                stream.node.timestamp = Date.now()
                            }
                            return bytesRead
                        },
                        write: (stream, buffer, offset, length, pos) => {
                            for (var i = 0; i < length; i++) {
                                try {
                                    output(buffer[offset + i])
                                } catch (e) {
                                    throw new FS.ErrnoError(29)
                                }
                            }
                            if (length) {
                                stream.node.timestamp = Date.now()
                            }
                            return i
                        }
                    });
                    return FS.mkdev(path, mode, dev)
                },
                forceLoadFile: obj => {
                    if (obj.isDevice || obj.isFolder || obj.link || obj.contents) return true;
                    if (typeof XMLHttpRequest !== "undefined") {
                        throw new Error("Lazy loading should have been performed (contents set) in createLazyFile, but it was not. Lazy loading only works in web workers. Use --embed-file or --preload-file in emcc on the main thread.")
                    } else if (read_) {
                        try {
                            obj.contents = intArrayFromString(read_(obj.url), true);
                            obj.usedBytes = obj.contents.length
                        } catch (e) {
                            throw new FS.ErrnoError(29)
                        }
                    } else {
                        throw new Error("Cannot load without read() or XMLHttpRequest.")
                    }
                },
                createLazyFile: (parent, name, url, canRead, canWrite) => {
                    function LazyUint8Array() {
                        this.lengthKnown = false;
                        this.chunks = []
                    }
                    LazyUint8Array.prototype.get = function LazyUint8Array_get(idx) {
                        if (idx > this.length - 1 || idx < 0) {
                            return undefined
                        }
                        var chunkOffset = idx % this.chunkSize;
                        var chunkNum = idx / this.chunkSize | 0;
                        return this.getter(chunkNum)[chunkOffset]
                    };
                    LazyUint8Array.prototype.setDataGetter = function LazyUint8Array_setDataGetter(getter) {
                        this.getter = getter
                    };
                    LazyUint8Array.prototype.cacheLength = function LazyUint8Array_cacheLength() {
                        var xhr = new XMLHttpRequest;
                        xhr.open("HEAD", url, false);
                        xhr.send(null);
                        if (!(xhr.status >= 200 && xhr.status < 300 || xhr.status === 304)) throw new Error("Couldn't load " + url + ". Status: " + xhr.status);
                        var datalength = Number(xhr.getResponseHeader("Content-length"));
                        var header;
                        var hasByteServing = (header = xhr.getResponseHeader("Accept-Ranges")) && header === "bytes";
                        var usesGzip = (header = xhr.getResponseHeader("Content-Encoding")) && header === "gzip";
                        var chunkSize = 1024 * 1024;
                        if (!hasByteServing) chunkSize = datalength;
                        var doXHR = (from, to) => {
                            if (from > to) throw new Error("invalid range (" + from + ", " + to + ") or no bytes requested!");
                            if (to > datalength - 1) throw new Error("only " + datalength + " bytes available! programmer error!");
                            var xhr = new XMLHttpRequest;
                            xhr.open("GET", url, false);
                            if (datalength !== chunkSize) xhr.setRequestHeader("Range", "bytes=" + from + "-" + to);
                            xhr.responseType = "arraybuffer";
                            if (xhr.overrideMimeType) {
                                xhr.overrideMimeType("text/plain; charset=x-user-defined")
                            }
                            xhr.send(null);
                            if (!(xhr.status >= 200 && xhr.status < 300 || xhr.status === 304)) throw new Error("Couldn't load " + url + ". Status: " + xhr.status);
                            if (xhr.response !== undefined) {
                                return new Uint8Array(xhr.response || [])
                            } else {
                                return intArrayFromString(xhr.responseText || "", true)
                            }
                        };
                        var lazyArray = this;
                        lazyArray.setDataGetter(chunkNum => {
                            var start = chunkNum * chunkSize;
                            var end = (chunkNum + 1) * chunkSize - 1;
                            end = Math.min(end, datalength - 1);
                            if (typeof lazyArray.chunks[chunkNum] === "undefined") {
                                lazyArray.chunks[chunkNum] = doXHR(start, end)
                            }
                            if (typeof lazyArray.chunks[chunkNum] === "undefined") throw new Error("doXHR failed!");
                            return lazyArray.chunks[chunkNum]
                        });
                        if (usesGzip || !datalength) {
                            chunkSize = datalength = 1;
                            datalength = this.getter(0).length;
                            chunkSize = datalength;
                            out("LazyFiles on gzip forces download of the whole file when length is accessed")
                        }
                        this._length = datalength;
                        this._chunkSize = chunkSize;
                        this.lengthKnown = true
                    };
                    if (typeof XMLHttpRequest !== "undefined") {
                        if (!ENVIRONMENT_IS_WORKER) throw "Cannot do synchronous binary XHRs outside webworkers in modern browsers. Use --embed-file or --preload-file in emcc";
                        var lazyArray = new LazyUint8Array;
                        Object.defineProperties(lazyArray, {
                            length: {
                                get: function() {
                                    if (!this.lengthKnown) {
                                        this.cacheLength()
                                    }
                                    return this._length
                                }
                            },
                            chunkSize: {
                                get: function() {
                                    if (!this.lengthKnown) {
                                        this.cacheLength()
                                    }
                                    return this._chunkSize
                                }
                            }
                        });
                        var properties = {
                            isDevice: false,
                            contents: lazyArray
                        }
                    } else {
                        var properties = {
                            isDevice: false,
                            url: url
                        }
                    }
                    var node = FS.createFile(parent, name, properties, canRead, canWrite);
                    if (properties.contents) {
                        node.contents = properties.contents
                    } else if (properties.url) {
                        node.contents = null;
                        node.url = properties.url
                    }
                    Object.defineProperties(node, {
                        usedBytes: {
                            get: function() {
                                return this.contents.length
                            }
                        }
                    });
                    var stream_ops = {};
                    var keys = Object.keys(node.stream_ops);
                    keys.forEach(key => {
                        var fn = node.stream_ops[key];
                        stream_ops[key] = function forceLoadLazyFile() {
                            FS.forceLoadFile(node);
                            return fn.apply(null, arguments)
                        }
                    });
                    stream_ops.read = ((stream, buffer, offset, length, position) => {
                        FS.forceLoadFile(node);
                        var contents = stream.node.contents;
                        if (position >= contents.length) return 0;
                        var size = Math.min(contents.length - position, length);
                        assert(size >= 0);
                        if (contents.slice) {
                            for (var i = 0; i < size; i++) {
                                buffer[offset + i] = contents[position + i]
                            }
                        } else {
                            for (var i = 0; i < size; i++) {
                                buffer[offset + i] = contents.get(position + i)
                            }
                        }
                        return size
                    });
                    node.stream_ops = stream_ops;
                    return node
                },
                createPreloadedFile: (parent, name, url, canRead, canWrite, onload, onerror, dontCreateFile, canOwn, preFinish) => {
                    var fullname = name ? PATH_FS.resolve(PATH.join2(parent, name)) : parent;
                    var dep = getUniqueRunDependency("cp " + fullname);

                    function processData(byteArray) {
                        function finish(byteArray) {
                            if (preFinish) preFinish();
                            if (!dontCreateFile) {
                                FS.createDataFile(parent, name, byteArray, canRead, canWrite, canOwn)
                            }
                            if (onload) onload();
                            removeRunDependency(dep)
                        }
                        if (Browser.handledByPreloadPlugin(byteArray, fullname, finish, () => {
                                if (onerror) onerror();
                                removeRunDependency(dep)
                            })) {
                            return
                        }
                        finish(byteArray)
                    }
                    addRunDependency(dep);
                    if (typeof url == "string") {
                        asyncLoad(url, byteArray => processData(byteArray), onerror)
                    } else {
                        processData(url)
                    }
                },
                indexedDB: () => {
                    return window.indexedDB || window.mozIndexedDB || window.webkitIndexedDB || window.msIndexedDB
                },
                DB_NAME: () => {
                    return "EM_FS_" + window.location.pathname
                },
                DB_VERSION: 20,
                DB_STORE_NAME: "FILE_DATA",
                saveFilesToDB: (paths, onload, onerror) => {
                    onload = onload || (() => {});
                    onerror = onerror || (() => {});
                    var indexedDB = FS.indexedDB();
                    try {
                        var openRequest = indexedDB.open(FS.DB_NAME(), FS.DB_VERSION)
                    } catch (e) {
                        return onerror(e)
                    }
                    openRequest.onupgradeneeded = (() => {
                        out("creating db");
                        var db = openRequest.result;
                        db.createObjectStore(FS.DB_STORE_NAME)
                    });
                    openRequest.onsuccess = (() => {
                        var db = openRequest.result;
                        var transaction = db.transaction([FS.DB_STORE_NAME], "readwrite");
                        var files = transaction.objectStore(FS.DB_STORE_NAME);
                        var ok = 0,
                            fail = 0,
                            total = paths.length;

                        function finish() {
                            if (fail == 0) onload();
                            else onerror()
                        }
                        paths.forEach(path => {
                            var putRequest = files.put(FS.analyzePath(path).object.contents, path);
                            putRequest.onsuccess = (() => {
                                ok++;
                                if (ok + fail == total) finish()
                            });
                            putRequest.onerror = (() => {
                                fail++;
                                if (ok + fail == total) finish()
                            })
                        });
                        transaction.onerror = onerror
                    });
                    openRequest.onerror = onerror
                },
                loadFilesFromDB: (paths, onload, onerror) => {
                    onload = onload || (() => {});
                    onerror = onerror || (() => {});
                    var indexedDB = FS.indexedDB();
                    try {
                        var openRequest = indexedDB.open(FS.DB_NAME(), FS.DB_VERSION)
                    } catch (e) {
                        return onerror(e)
                    }
                    openRequest.onupgradeneeded = onerror;
                    openRequest.onsuccess = (() => {
                        var db = openRequest.result;
                        try {
                            var transaction = db.transaction([FS.DB_STORE_NAME], "readonly")
                        } catch (e) {
                            onerror(e);
                            return
                        }
                        var files = transaction.objectStore(FS.DB_STORE_NAME);
                        var ok = 0,
                            fail = 0,
                            total = paths.length;

                        function finish() {
                            if (fail == 0) onload();
                            else onerror()
                        }
                        paths.forEach(path => {
                            var getRequest = files.get(path);
                            getRequest.onsuccess = (() => {
                                if (FS.analyzePath(path).exists) {
                                    FS.unlink(path)
                                }
                                FS.createDataFile(PATH.dirname(path), PATH.basename(path), getRequest.result, true, true, true);
                                ok++;
                                if (ok + fail == total) finish()
                            });
                            getRequest.onerror = (() => {
                                fail++;
                                if (ok + fail == total) finish()
                            })
                        });
                        transaction.onerror = onerror
                    });
                    openRequest.onerror = onerror
                },
                absolutePath: () => {
                    abort("FS.absolutePath has been removed; use PATH_FS.resolve instead")
                },
                createFolder: () => {
                    abort("FS.createFolder has been removed; use FS.mkdir instead")
                },
                createLink: () => {
                    abort("FS.createLink has been removed; use FS.symlink instead")
                },
                joinPath: () => {
                    abort("FS.joinPath has been removed; use PATH.join instead")
                },
                mmapAlloc: () => {
                    abort("FS.mmapAlloc has been replaced by the top level function mmapAlloc")
                },
                standardizePath: () => {
                    abort("FS.standardizePath has been removed; use PATH.normalize instead")
                }
            };
            var SYSCALLS = {
                mappings: {},
                DEFAULT_POLLMASK: 5,
                calculateAt: function(dirfd, path, allowEmpty) {
                    if (path[0] === "/") {
                        return path
                    }
                    var dir;
                    if (dirfd === -100) {
                        dir = FS.cwd()
                    } else {
                        var dirstream = FS.getStream(dirfd);
                        if (!dirstream) throw new FS.ErrnoError(8);
                        dir = dirstream.path
                    }
                    if (path.length == 0) {
                        if (!allowEmpty) {
                            throw new FS.ErrnoError(44)
                        }
                        return dir
                    }
                    return PATH.join2(dir, path)
                },
                doStat: function(func, path, buf) {
                    try {
                        var stat = func(path)
                    } catch (e) {
                        if (e && e.node && PATH.normalize(path) !== PATH.normalize(FS.getPath(e.node))) {
                            return -54
                        }
                        throw e
                    }
                    HEAP32[buf >> 2] = stat.dev;
                    HEAP32[buf + 4 >> 2] = 0;
                    HEAP32[buf + 8 >> 2] = stat.ino;
                    HEAP32[buf + 12 >> 2] = stat.mode;
                    HEAP32[buf + 16 >> 2] = stat.nlink;
                    HEAP32[buf + 20 >> 2] = stat.uid;
                    HEAP32[buf + 24 >> 2] = stat.gid;
                    HEAP32[buf + 28 >> 2] = stat.rdev;
                    HEAP32[buf + 32 >> 2] = 0;
                    tempI64 = [stat.size >>> 0, (tempDouble = stat.size, +Math.abs(tempDouble) >= 1 ? tempDouble > 0 ? (Math.min(+Math.floor(tempDouble / 4294967296), 4294967295) | 0) >>> 0 : ~~+Math.ceil((tempDouble - +(~~tempDouble >>> 0)) / 4294967296) >>> 0 : 0)], HEAP32[buf + 40 >> 2] = tempI64[0], HEAP32[buf + 44 >> 2] = tempI64[1];
                    HEAP32[buf + 48 >> 2] = 4096;
                    HEAP32[buf + 52 >> 2] = stat.blocks;
                    HEAP32[buf + 56 >> 2] = stat.atime.getTime() / 1e3 | 0;
                    HEAP32[buf + 60 >> 2] = 0;
                    HEAP32[buf + 64 >> 2] = stat.mtime.getTime() / 1e3 | 0;
                    HEAP32[buf + 68 >> 2] = 0;
                    HEAP32[buf + 72 >> 2] = stat.ctime.getTime() / 1e3 | 0;
                    HEAP32[buf + 76 >> 2] = 0;
                    tempI64 = [stat.ino >>> 0, (tempDouble = stat.ino, +Math.abs(tempDouble) >= 1 ? tempDouble > 0 ? (Math.min(+Math.floor(tempDouble / 4294967296), 4294967295) | 0) >>> 0 : ~~+Math.ceil((tempDouble - +(~~tempDouble >>> 0)) / 4294967296) >>> 0 : 0)], HEAP32[buf + 80 >> 2] = tempI64[0], HEAP32[buf + 84 >> 2] = tempI64[1];
                    return 0
                },
                doMsync: function(addr, stream, len, flags, offset) {
                    var buffer = HEAPU8.slice(addr, addr + len);
                    FS.msync(stream, buffer, offset, len, flags)
                },
                doMkdir: function(path, mode) {
                    path = PATH.normalize(path);
                    if (path[path.length - 1] === "/") path = path.substr(0, path.length - 1);
                    FS.mkdir(path, mode, 0);
                    return 0
                },
                doMknod: function(path, mode, dev) {
                    switch (mode & 61440) {
                        case 32768:
                        case 8192:
                        case 24576:
                        case 4096:
                        case 49152:
                            break;
                        default:
                            return -28
                    }
                    FS.mknod(path, mode, dev);
                    return 0
                },
                doReadlink: function(path, buf, bufsize) {
                    if (bufsize <= 0) return -28;
                    var ret = FS.readlink(path);
                    var len = Math.min(bufsize, lengthBytesUTF8(ret));
                    var endChar = HEAP8[buf + len];
                    stringToUTF8(ret, buf, bufsize + 1);
                    HEAP8[buf + len] = endChar;
                    return len
                },
                doAccess: function(path, amode) {
                    if (amode & ~7) {
                        return -28
                    }
                    var lookup = FS.lookupPath(path, {
                        follow: true
                    });
                    var node = lookup.node;
                    if (!node) {
                        return -44
                    }
                    var perms = "";
                    if (amode & 4) perms += "r";
                    if (amode & 2) perms += "w";
                    if (amode & 1) perms += "x";
                    if (perms && FS.nodePermissions(node, perms)) {
                        return -2
                    }
                    return 0
                },
                doDup: function(path, flags, suggestFD) {
                    var suggest = FS.getStream(suggestFD);
                    if (suggest) FS.close(suggest);
                    return FS.open(path, flags, 0, suggestFD, suggestFD).fd
                },
                doReadv: function(stream, iov, iovcnt, offset) {
                    var ret = 0;
                    for (var i = 0; i < iovcnt; i++) {
                        var ptr = HEAP32[iov + i * 8 >> 2];
                        var len = HEAP32[iov + (i * 8 + 4) >> 2];
                        var curr = FS.read(stream, HEAP8, ptr, len, offset);
                        if (curr < 0) return -1;
                        ret += curr;
                        if (curr < len) break
                    }
                    return ret
                },
                doWritev: function(stream, iov, iovcnt, offset) {
                    var ret = 0;
                    for (var i = 0; i < iovcnt; i++) {
                        var ptr = HEAP32[iov + i * 8 >> 2];
                        var len = HEAP32[iov + (i * 8 + 4) >> 2];
                        var curr = FS.write(stream, HEAP8, ptr, len, offset);
                        if (curr < 0) return -1;
                        ret += curr
                    }
                    return ret
                },
                varargs: undefined,
                get: function() {
                    assert(SYSCALLS.varargs != undefined);
                    SYSCALLS.varargs += 4;
                    var ret = HEAP32[SYSCALLS.varargs - 4 >> 2];
                    return ret
                },
                getStr: function(ptr) {
                    var ret = UTF8ToString(ptr);
                    return ret
                },
                getStreamFromFD: function(fd) {
                    var stream = FS.getStream(fd);
                    if (!stream) throw new FS.ErrnoError(8);
                    return stream
                },
                get64: function(low, high) {
                    if (low >= 0) assert(high === 0);
                    else assert(high === -1);
                    return low
                }
            };

            function ___syscall_access(path, amode) {
                try {
                    path = SYSCALLS.getStr(path);
                    return SYSCALLS.doAccess(path, amode)
                } catch (e) {
                    if (typeof FS === "undefined" || !(e instanceof FS.ErrnoError)) throw e;
                    return -e.errno
                }
            }

            function ___syscall_chdir(path) {
                try {
                    path = SYSCALLS.getStr(path);
                    FS.chdir(path);
                    return 0
                } catch (e) {
                    if (typeof FS === "undefined" || !(e instanceof FS.ErrnoError)) throw e;
                    return -e.errno
                }
            }

            function ___syscall_chmod(path, mode) {
                try {
                    path = SYSCALLS.getStr(path);
                    FS.chmod(path, mode);
                    return 0
                } catch (e) {
                    if (typeof FS === "undefined" || !(e instanceof FS.ErrnoError)) throw e;
                    return -e.errno
                }
            }

            function ___syscall_dup3(fd, suggestFD, flags) {
                try {
                    var old = SYSCALLS.getStreamFromFD(fd);
                    assert(!flags);
                    if (old.fd === suggestFD) return -28;
                    return SYSCALLS.doDup(old.path, old.flags, suggestFD)
                } catch (e) {
                    if (typeof FS === "undefined" || !(e instanceof FS.ErrnoError)) throw e;
                    return -e.errno
                }
            }

            function setErrNo(value) {
                HEAP32[___errno_location() >> 2] = value;
                return value
            }

            function ___syscall_fcntl64(fd, cmd, varargs) {
                SYSCALLS.varargs = varargs;
                try {
                    var stream = SYSCALLS.getStreamFromFD(fd);
                    switch (cmd) {
                        case 0: {
                            var arg = SYSCALLS.get();
                            if (arg < 0) {
                                return -28
                            }
                            var newStream;
                            newStream = FS.open(stream.path, stream.flags, 0, arg);
                            return newStream.fd
                        }
                        case 1:
                        case 2:
                            return 0;
                        case 3:
                            return stream.flags;
                        case 4: {
                            var arg = SYSCALLS.get();
                            stream.flags |= arg;
                            return 0
                        }
                        case 5: {
                            var arg = SYSCALLS.get();
                            var offset = 0;
                            HEAP16[arg + offset >> 1] = 2;
                            return 0
                        }
                        case 6:
                        case 7:
                            return 0;
                        case 16:
                        case 8:
                            return -28;
                        case 9:
                            setErrNo(28);
                            return -1;
                        default: {
                            return -28
                        }
                    }
                } catch (e) {
                    if (typeof FS === "undefined" || !(e instanceof FS.ErrnoError)) throw e;
                    return -e.errno
                }
            }

            function ___syscall_fstat64(fd, buf) {
                try {
                    var stream = SYSCALLS.getStreamFromFD(fd);
                    return SYSCALLS.doStat(FS.stat, stream.path, buf)
                } catch (e) {
                    if (typeof FS === "undefined" || !(e instanceof FS.ErrnoError)) throw e;
                    return -e.errno
                }
            }

            function ___syscall_fstatat64(dirfd, path, buf, flags) {
                try {
                    path = SYSCALLS.getStr(path);
                    var nofollow = flags & 256;
                    var allowEmpty = flags & 4096;
                    flags = flags & ~4352;
                    assert(!flags, flags);
                    path = SYSCALLS.calculateAt(dirfd, path, allowEmpty);
                    return SYSCALLS.doStat(nofollow ? FS.lstat : FS.stat, path, buf)
                } catch (e) {
                    if (typeof FS === "undefined" || !(e instanceof FS.ErrnoError)) throw e;
                    return -e.errno
                }
            }

            function ___syscall_ftruncate64(fd, low, high) {
                try {
                    var length = SYSCALLS.get64(low, high);
                    FS.ftruncate(fd, length);
                    return 0
                } catch (e) {
                    if (typeof FS === "undefined" || !(e instanceof FS.ErrnoError)) throw e;
                    return -e.errno
                }
            }

            function ___syscall_getcwd(buf, size) {
                try {
                    if (size === 0) return -28;
                    var cwd = FS.cwd();
                    var cwdLengthInBytes = lengthBytesUTF8(cwd);
                    if (size < cwdLengthInBytes + 1) return -68;
                    stringToUTF8(cwd, buf, size);
                    return buf
                } catch (e) {
                    if (typeof FS === "undefined" || !(e instanceof FS.ErrnoError)) throw e;
                    return -e.errno
                }
            }

            function ___syscall_getdents64(fd, dirp, count) {
                try {
                    var stream = SYSCALLS.getStreamFromFD(fd);
                    if (!stream.getdents) {
                        stream.getdents = FS.readdir(stream.path)
                    }
                    var struct_size = 280;
                    var pos = 0;
                    var off = FS.llseek(stream, 0, 1);
                    var idx = Math.floor(off / struct_size);
                    while (idx < stream.getdents.length && pos + struct_size <= count) {
                        var id;
                        var type;
                        var name = stream.getdents[idx];
                        if (name === ".") {
                            id = stream.node.id;
                            type = 4
                        } else if (name === "..") {
                            var lookup = FS.lookupPath(stream.path, {
                                parent: true
                            });
                            id = lookup.node.id;
                            type = 4
                        } else {
                            var child = FS.lookupNode(stream.node, name);
                            id = child.id;
                            type = FS.isChrdev(child.mode) ? 2 : FS.isDir(child.mode) ? 4 : FS.isLink(child.mode) ? 10 : 8
                        }
                        assert(id);
                        tempI64 = [id >>> 0, (tempDouble = id, +Math.abs(tempDouble) >= 1 ? tempDouble > 0 ? (Math.min(+Math.floor(tempDouble / 4294967296), 4294967295) | 0) >>> 0 : ~~+Math.ceil((tempDouble - +(~~tempDouble >>> 0)) / 4294967296) >>> 0 : 0)], HEAP32[dirp + pos >> 2] = tempI64[0], HEAP32[dirp + pos + 4 >> 2] = tempI64[1];
                        tempI64 = [(idx + 1) * struct_size >>> 0, (tempDouble = (idx + 1) * struct_size, +Math.abs(tempDouble) >= 1 ? tempDouble > 0 ? (Math.min(+Math.floor(tempDouble / 4294967296), 4294967295) | 0) >>> 0 : ~~+Math.ceil((tempDouble - +(~~tempDouble >>> 0)) / 4294967296) >>> 0 : 0)], HEAP32[dirp + pos + 8 >> 2] = tempI64[0], HEAP32[dirp + pos + 12 >> 2] = tempI64[1];
                        HEAP16[dirp + pos + 16 >> 1] = 280;
                        HEAP8[dirp + pos + 18 >> 0] = type;
                        stringToUTF8(name, dirp + pos + 19, 256);
                        pos += struct_size;
                        idx += 1
                    }
                    FS.llseek(stream, idx * struct_size, 0);
                    return pos
                } catch (e) {
                    if (typeof FS === "undefined" || !(e instanceof FS.ErrnoError)) throw e;
                    return -e.errno
                }
            }

            function ___syscall_getegid32() {
                return 0
            }

            function ___syscall_getuid32() {
                return ___syscall_getegid32()
            }

            function ___syscall_ioctl(fd, op, varargs) {
                SYSCALLS.varargs = varargs;
                try {
                    var stream = SYSCALLS.getStreamFromFD(fd);
                    switch (op) {
                        case 21509:
                        case 21505: {
                            if (!stream.tty) return -59;
                            return 0
                        }
                        case 21510:
                        case 21511:
                        case 21512:
                        case 21506:
                        case 21507:
                        case 21508: {
                            if (!stream.tty) return -59;
                            return 0
                        }
                        case 21519: {
                            if (!stream.tty) return -59;
                            var argp = SYSCALLS.get();
                            HEAP32[argp >> 2] = 0;
                            return 0
                        }
                        case 21520: {
                            if (!stream.tty) return -59;
                            return -28
                        }
                        case 21531: {
                            var argp = SYSCALLS.get();
                            return FS.ioctl(stream, op, argp)
                        }
                        case 21523: {
                            if (!stream.tty) return -59;
                            return 0
                        }
                        case 21524: {
                            if (!stream.tty) return -59;
                            return 0
                        }
                        default:
                            abort("bad ioctl syscall " + op)
                    }
                } catch (e) {
                    if (typeof FS === "undefined" || !(e instanceof FS.ErrnoError)) throw e;
                    return -e.errno
                }
            }

            function ___syscall_lstat64(path, buf) {
                try {
                    path = SYSCALLS.getStr(path);
                    return SYSCALLS.doStat(FS.lstat, path, buf)
                } catch (e) {
                    if (typeof FS === "undefined" || !(e instanceof FS.ErrnoError)) throw e;
                    return -e.errno
                }
            }

            function ___syscall_mkdir(path, mode) {
                try {
                    path = SYSCALLS.getStr(path);
                    return SYSCALLS.doMkdir(path, mode)
                } catch (e) {
                    if (typeof FS === "undefined" || !(e instanceof FS.ErrnoError)) throw e;
                    return -e.errno
                }
            }

            function syscallMmap2(addr, len, prot, flags, fd, off) {
                off <<= 12;
                var ptr;
                var allocated = false;
                if ((flags & 16) !== 0 && addr % 65536 !== 0) {
                    return -28
                }
                if ((flags & 32) !== 0) {
                    ptr = mmapAlloc(len);
                    if (!ptr) return -48;
                    allocated = true
                } else {
                    var info = FS.getStream(fd);
                    if (!info) return -8;
                    var res = FS.mmap(info, addr, len, off, prot, flags);
                    ptr = res.ptr;
                    allocated = res.allocated
                }
                SYSCALLS.mappings[ptr] = {
                    malloc: ptr,
                    len: len,
                    allocated: allocated,
                    fd: fd,
                    prot: prot,
                    flags: flags,
                    offset: off
                };
                return ptr
            }

            function ___syscall_mmap2(addr, len, prot, flags, fd, off) {
                try {
                    return syscallMmap2(addr, len, prot, flags, fd, off)
                } catch (e) {
                    if (typeof FS === "undefined" || !(e instanceof FS.ErrnoError)) throw e;
                    return -e.errno
                }
            }

            function syscallMunmap(addr, len) {
                var info = SYSCALLS.mappings[addr];
                if (len === 0 || !info) {
                    return -28
                }
                if (len === info.len) {
                    var stream = FS.getStream(info.fd);
                    if (stream) {
                        if (info.prot & 2) {
                            SYSCALLS.doMsync(addr, stream, len, info.flags, info.offset)
                        }
                        FS.munmap(stream)
                    }
                    SYSCALLS.mappings[addr] = null;
                    if (info.allocated) {
                        _free(info.malloc)
                    }
                }
                return 0
            }

            function ___syscall_munmap(addr, len) {
                try {
                    return syscallMunmap(addr, len)
                } catch (e) {
                    if (typeof FS === "undefined" || !(e instanceof FS.ErrnoError)) throw e;
                    return -e.errno
                }
            }

            function ___syscall_open(path, flags, varargs) {
                SYSCALLS.varargs = varargs;
                try {
                    var pathname = SYSCALLS.getStr(path);
                    var mode = varargs ? SYSCALLS.get() : 0;
                    var stream = FS.open(pathname, flags, mode);
                    return stream.fd
                } catch (e) {
                    if (typeof FS === "undefined" || !(e instanceof FS.ErrnoError)) throw e;
                    return -e.errno
                }
            }

            function ___syscall_poll(fds, nfds, timeout) {
                try {
                    var nonzero = 0;
                    for (var i = 0; i < nfds; i++) {
                        var pollfd = fds + 8 * i;
                        var fd = HEAP32[pollfd >> 2];
                        var events = HEAP16[pollfd + 4 >> 1];
                        var mask = 32;
                        var stream = FS.getStream(fd);
                        if (stream) {
                            mask = SYSCALLS.DEFAULT_POLLMASK;
                            if (stream.stream_ops.poll) {
                                mask = stream.stream_ops.poll(stream)
                            }
                        }
                        mask &= events | 8 | 16;
                        if (mask) nonzero++;
                        HEAP16[pollfd + 6 >> 1] = mask
                    }
                    return nonzero
                } catch (e) {
                    if (typeof FS === "undefined" || !(e instanceof FS.ErrnoError)) throw e;
                    return -e.errno
                }
            }

            function ___syscall_readlink(path, buf, bufsize) {
                try {
                    path = SYSCALLS.getStr(path);
                    return SYSCALLS.doReadlink(path, buf, bufsize)
                } catch (e) {
                    if (typeof FS === "undefined" || !(e instanceof FS.ErrnoError)) throw e;
                    return -e.errno
                }
            }

            function ___syscall_rename(old_path, new_path) {
                try {
                    old_path = SYSCALLS.getStr(old_path);
                    new_path = SYSCALLS.getStr(new_path);
                    FS.rename(old_path, new_path);
                    return 0
                } catch (e) {
                    if (typeof FS === "undefined" || !(e instanceof FS.ErrnoError)) throw e;
                    return -e.errno
                }
            }

            function ___syscall_rmdir(path) {
                try {
                    path = SYSCALLS.getStr(path);
                    FS.rmdir(path);
                    return 0
                } catch (e) {
                    if (typeof FS === "undefined" || !(e instanceof FS.ErrnoError)) throw e;
                    return -e.errno
                }
            }

            function ___syscall_stat64(path, buf) {
                try {
                    path = SYSCALLS.getStr(path);
                    return SYSCALLS.doStat(FS.stat, path, buf)
                } catch (e) {
                    if (typeof FS === "undefined" || !(e instanceof FS.ErrnoError)) throw e;
                    return -e.errno
                }
            }

            function ___syscall_unlink(path) {
                try {
                    path = SYSCALLS.getStr(path);
                    FS.unlink(path);
                    return 0
                } catch (e) {
                    if (typeof FS === "undefined" || !(e instanceof FS.ErrnoError)) throw e;
                    return -e.errno
                }
            }

            function __dlopen_js(filename, flag) {
                abort("To use dlopen, you need to use Emscripten's linking support, see https://github.com/emscripten-core/emscripten/wiki/Linking")
            }

            function __dlsym_js(handle, symbol) {
                abort("To use dlopen, you need to use Emscripten's linking support, see https://github.com/emscripten-core/emscripten/wiki/Linking")
            }

            function __emscripten_throw_longjmp() {
                throw "longjmp"
            }

            function __localtime_js(time, tmPtr) {
                var date = new Date(HEAP32[time >> 2] * 1e3);
                HEAP32[tmPtr >> 2] = date.getSeconds();
                HEAP32[tmPtr + 4 >> 2] = date.getMinutes();
                HEAP32[tmPtr + 8 >> 2] = date.getHours();
                HEAP32[tmPtr + 12 >> 2] = date.getDate();
                HEAP32[tmPtr + 16 >> 2] = date.getMonth();
                HEAP32[tmPtr + 20 >> 2] = date.getFullYear() - 1900;
                HEAP32[tmPtr + 24 >> 2] = date.getDay();
                var start = new Date(date.getFullYear(), 0, 1);
                var yday = (date.getTime() - start.getTime()) / (1e3 * 60 * 60 * 24) | 0;
                HEAP32[tmPtr + 28 >> 2] = yday;
                HEAP32[tmPtr + 36 >> 2] = -(date.getTimezoneOffset() * 60);
                var summerOffset = new Date(date.getFullYear(), 6, 1).getTimezoneOffset();
                var winterOffset = start.getTimezoneOffset();
                var dst = (summerOffset != winterOffset && date.getTimezoneOffset() == Math.min(winterOffset, summerOffset)) | 0;
                HEAP32[tmPtr + 32 >> 2] = dst
            }

            function __mktime_js(tmPtr) {
                var date = new Date(HEAP32[tmPtr + 20 >> 2] + 1900, HEAP32[tmPtr + 16 >> 2], HEAP32[tmPtr + 12 >> 2], HEAP32[tmPtr + 8 >> 2], HEAP32[tmPtr + 4 >> 2], HEAP32[tmPtr >> 2], 0);
                var dst = HEAP32[tmPtr + 32 >> 2];
                var guessedOffset = date.getTimezoneOffset();
                var start = new Date(date.getFullYear(), 0, 1);
                var summerOffset = new Date(date.getFullYear(), 6, 1).getTimezoneOffset();
                var winterOffset = start.getTimezoneOffset();
                var dstOffset = Math.min(winterOffset, summerOffset);
                if (dst < 0) {
                    HEAP32[tmPtr + 32 >> 2] = Number(summerOffset != winterOffset && dstOffset == guessedOffset)
                } else if (dst > 0 != (dstOffset == guessedOffset)) {
                    var nonDstOffset = Math.max(winterOffset, summerOffset);
                    var trueOffset = dst > 0 ? dstOffset : nonDstOffset;
                    date.setTime(date.getTime() + (trueOffset - guessedOffset) * 6e4)
                }
                HEAP32[tmPtr + 24 >> 2] = date.getDay();
                var yday = (date.getTime() - start.getTime()) / (1e3 * 60 * 60 * 24) | 0;
                HEAP32[tmPtr + 28 >> 2] = yday;
                HEAP32[tmPtr >> 2] = date.getSeconds();
                HEAP32[tmPtr + 4 >> 2] = date.getMinutes();
                HEAP32[tmPtr + 8 >> 2] = date.getHours();
                HEAP32[tmPtr + 12 >> 2] = date.getDate();
                HEAP32[tmPtr + 16 >> 2] = date.getMonth();
                return date.getTime() / 1e3 | 0
            }

            function _tzset_impl(timezone, daylight, tzname) {
                var currentYear = (new Date).getFullYear();
                var winter = new Date(currentYear, 0, 1);
                var summer = new Date(currentYear, 6, 1);
                var winterOffset = winter.getTimezoneOffset();
                var summerOffset = summer.getTimezoneOffset();
                var stdTimezoneOffset = Math.max(winterOffset, summerOffset);
                HEAP32[timezone >> 2] = stdTimezoneOffset * 60;
                HEAP32[daylight >> 2] = Number(winterOffset != summerOffset);

                function extractZone(date) {
                    var match = date.toTimeString().match(/\(([A-Za-z ]+)\)$/);
                    return match ? match[1] : "GMT"
                }
                var winterName = extractZone(winter);
                var summerName = extractZone(summer);
                var winterNamePtr = allocateUTF8(winterName);
                var summerNamePtr = allocateUTF8(summerName);
                if (summerOffset < winterOffset) {
                    HEAP32[tzname >> 2] = winterNamePtr;
                    HEAP32[tzname + 4 >> 2] = summerNamePtr
                } else {
                    HEAP32[tzname >> 2] = summerNamePtr;
                    HEAP32[tzname + 4 >> 2] = winterNamePtr
                }
            }

            function __tzset_js(timezone, daylight, tzname) {
                if (__tzset_js.called) return;
                __tzset_js.called = true;
                _tzset_impl(timezone, daylight, tzname)
            }

            function _abort() {
                abort("native code called abort()")
            }
            var _emscripten_get_now;
            if (ENVIRONMENT_IS_NODE) {
                _emscripten_get_now = (() => {
                    var t = process["hrtime"]();
                    return t[0] * 1e3 + t[1] / 1e6
                })
            } else _emscripten_get_now = (() => performance.now());
            var _emscripten_get_now_is_monotonic = true;

            function _clock_gettime(clk_id, tp) {
                var now;
                if (clk_id === 0) {
                    now = Date.now()
                } else if ((clk_id === 1 || clk_id === 4) && _emscripten_get_now_is_monotonic) {
                    now = _emscripten_get_now()
                } else {
                    setErrNo(28);
                    return -1
                }
                HEAP32[tp >> 2] = now / 1e3 | 0;
                HEAP32[tp + 4 >> 2] = now % 1e3 * 1e3 * 1e3 | 0;
                return 0
            }

            function _emscripten_console_error(str) {
                assert(typeof str === "number");
                console.error(UTF8ToString(str))
            }

            function _emscripten_get_heap_max() {
                return 2147483648
            }

            function _emscripten_memcpy_big(dest, src, num) {
                HEAPU8.copyWithin(dest, src, src + num)
            }

            function emscripten_realloc_buffer(size) {
                try {
                    wasmMemory.grow(size - buffer.byteLength + 65535 >>> 16);
                    updateGlobalBufferAndViews(wasmMemory.buffer);
                    return 1
                } catch (e) {
                    err("emscripten_realloc_buffer: Attempted to grow heap from " + buffer.byteLength + " bytes to " + size + " bytes, but got error: " + e)
                }
            }

            function _emscripten_resize_heap(requestedSize) {
                var oldSize = HEAPU8.length;
                requestedSize = requestedSize >>> 0;
                assert(requestedSize > oldSize);
                var maxHeapSize = _emscripten_get_heap_max();
                if (requestedSize > maxHeapSize) {
                    err("Cannot enlarge memory, asked to go up to " + requestedSize + " bytes, but the limit is " + maxHeapSize + " bytes!");
                    return false
                }
                for (var cutDown = 1; cutDown <= 4; cutDown *= 2) {
                    var overGrownHeapSize = oldSize * (1 + .2 / cutDown);
                    overGrownHeapSize = Math.min(overGrownHeapSize, requestedSize + 100663296);
                    var newSize = Math.min(maxHeapSize, alignUp(Math.max(requestedSize, overGrownHeapSize), 65536));
                    var replacement = emscripten_realloc_buffer(newSize);
                    if (replacement) {
                        return true
                    }
                }
                err("Failed to grow the heap from " + oldSize + " bytes to " + newSize + " bytes, not enough memory!");
                return false
            }
            var ENV = {};

            function getExecutableName() {
                return thisProgram || "./this.program"
            }

            function getEnvStrings() {
                if (!getEnvStrings.strings) {
                    var lang = (typeof navigator === "object" && navigator.languages && navigator.languages[0] || "C").replace("-", "_") + ".UTF-8";
                    var env = {
                        "USER": "web_user",
                        "LOGNAME": "web_user",
                        "PATH": "/",
                        "PWD": "/",
                        "HOME": "/home/web_user",
                        "LANG": lang,
                        "_": getExecutableName()
                    };
                    for (var x in ENV) {
                        if (ENV[x] === undefined) delete env[x];
                        else env[x] = ENV[x]
                    }
                    var strings = [];
                    for (var x in env) {
                        strings.push(x + "=" + env[x])
                    }
                    getEnvStrings.strings = strings
                }
                return getEnvStrings.strings
            }

            function _environ_get(__environ, environ_buf) {
                var bufSize = 0;
                getEnvStrings().forEach(function(string, i) {
                    var ptr = environ_buf + bufSize;
                    HEAP32[__environ + i * 4 >> 2] = ptr;
                    writeAsciiToMemory(string, ptr);
                    bufSize += string.length + 1
                });
                return 0
            }

            function _environ_sizes_get(penviron_count, penviron_buf_size) {
                var strings = getEnvStrings();
                HEAP32[penviron_count >> 2] = strings.length;
                var bufSize = 0;
                strings.forEach(function(string) {
                    bufSize += string.length + 1
                });
                HEAP32[penviron_buf_size >> 2] = bufSize;
                return 0
            }

            function _exit(status) {
                exit(status)
            }

            function _fd_close(fd) {
                try {
                    var stream = SYSCALLS.getStreamFromFD(fd);
                    FS.close(stream);
                    return 0
                } catch (e) {
                    if (typeof FS === "undefined" || !(e instanceof FS.ErrnoError)) throw e;
                    return e.errno
                }
            }

            function _fd_fdstat_get(fd, pbuf) {
                try {
                    var stream = SYSCALLS.getStreamFromFD(fd);
                    var type = stream.tty ? 2 : FS.isDir(stream.mode) ? 3 : FS.isLink(stream.mode) ? 7 : 4;
                    HEAP8[pbuf >> 0] = type;
                    return 0
                } catch (e) {
                    if (typeof FS === "undefined" || !(e instanceof FS.ErrnoError)) throw e;
                    return e.errno
                }
            }

            function _fd_read(fd, iov, iovcnt, pnum) {
                try {
                    var stream = SYSCALLS.getStreamFromFD(fd);
                    var num = SYSCALLS.doReadv(stream, iov, iovcnt);
                    HEAP32[pnum >> 2] = num;
                    return 0
                } catch (e) {
                    if (typeof FS === "undefined" || !(e instanceof FS.ErrnoError)) throw e;
                    return e.errno
                }
            }

            function _fd_seek(fd, offset_low, offset_high, whence, newOffset) {
                try {
                    var stream = SYSCALLS.getStreamFromFD(fd);
                    var HIGH_OFFSET = 4294967296;
                    var offset = offset_high * HIGH_OFFSET + (offset_low >>> 0);
                    var DOUBLE_LIMIT = 9007199254740992;
                    if (offset <= -DOUBLE_LIMIT || offset >= DOUBLE_LIMIT) {
                        return -61
                    }
                    FS.llseek(stream, offset, whence);
                    tempI64 = [stream.position >>> 0, (tempDouble = stream.position, +Math.abs(tempDouble) >= 1 ? tempDouble > 0 ? (Math.min(+Math.floor(tempDouble / 4294967296), 4294967295) | 0) >>> 0 : ~~+Math.ceil((tempDouble - +(~~tempDouble >>> 0)) / 4294967296) >>> 0 : 0)], HEAP32[newOffset >> 2] = tempI64[0], HEAP32[newOffset + 4 >> 2] = tempI64[1];
                    if (stream.getdents && offset === 0 && whence === 0) stream.getdents = null;
                    return 0
                } catch (e) {
                    if (typeof FS === "undefined" || !(e instanceof FS.ErrnoError)) throw e;
                    return e.errno
                }
            }

            function _fd_write(fd, iov, iovcnt, pnum) {
                try {
                    var stream = SYSCALLS.getStreamFromFD(fd);
                    var num = SYSCALLS.doWritev(stream, iov, iovcnt);
                    HEAP32[pnum >> 2] = num;
                    return 0
                } catch (e) {
                    if (typeof FS === "undefined" || !(e instanceof FS.ErrnoError)) throw e;
                    return e.errno
                }
            }

            function _getTempRet0() {
                return getTempRet0()
            }

            function _gettimeofday(ptr) {
                var now = Date.now();
                HEAP32[ptr >> 2] = now / 1e3 | 0;
                HEAP32[ptr + 4 >> 2] = now % 1e3 * 1e3 | 0;
                return 0
            }

            function _setTempRet0(val) {
                setTempRet0(val)
            }

            function __isLeapYear(year) {
                return year % 4 === 0 && (year % 100 !== 0 || year % 400 === 0)
            }

            function __arraySum(array, index) {
                var sum = 0;
                for (var i = 0; i <= index; sum += array[i++]) {}
                return sum
            }
            var __MONTH_DAYS_LEAP = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
            var __MONTH_DAYS_REGULAR = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];

            function __addDays(date, days) {
                var newDate = new Date(date.getTime());
                while (days > 0) {
                    var leap = __isLeapYear(newDate.getFullYear());
                    var currentMonth = newDate.getMonth();
                    var daysInCurrentMonth = (leap ? __MONTH_DAYS_LEAP : __MONTH_DAYS_REGULAR)[currentMonth];
                    if (days > daysInCurrentMonth - newDate.getDate()) {
                        days -= daysInCurrentMonth - newDate.getDate() + 1;
                        newDate.setDate(1);
                        if (currentMonth < 11) {
                            newDate.setMonth(currentMonth + 1)
                        } else {
                            newDate.setMonth(0);
                            newDate.setFullYear(newDate.getFullYear() + 1)
                        }
                    } else {
                        newDate.setDate(newDate.getDate() + days);
                        return newDate
                    }
                }
                return newDate
            }

            function _strftime(s, maxsize, format, tm) {
                var tm_zone = HEAP32[tm + 40 >> 2];
                var date = {
                    tm_sec: HEAP32[tm >> 2],
                    tm_min: HEAP32[tm + 4 >> 2],
                    tm_hour: HEAP32[tm + 8 >> 2],
                    tm_mday: HEAP32[tm + 12 >> 2],
                    tm_mon: HEAP32[tm + 16 >> 2],
                    tm_year: HEAP32[tm + 20 >> 2],
                    tm_wday: HEAP32[tm + 24 >> 2],
                    tm_yday: HEAP32[tm + 28 >> 2],
                    tm_isdst: HEAP32[tm + 32 >> 2],
                    tm_gmtoff: HEAP32[tm + 36 >> 2],
                    tm_zone: tm_zone ? UTF8ToString(tm_zone) : ""
                };
                var pattern = UTF8ToString(format);
                var EXPANSION_RULES_1 = {
                    "%c": "%a %b %d %H:%M:%S %Y",
                    "%D": "%m/%d/%y",
                    "%F": "%Y-%m-%d",
                    "%h": "%b",
                    "%r": "%I:%M:%S %p",
                    "%R": "%H:%M",
                    "%T": "%H:%M:%S",
                    "%x": "%m/%d/%y",
                    "%X": "%H:%M:%S",
                    "%Ec": "%c",
                    "%EC": "%C",
                    "%Ex": "%m/%d/%y",
                    "%EX": "%H:%M:%S",
                    "%Ey": "%y",
                    "%EY": "%Y",
                    "%Od": "%d",
                    "%Oe": "%e",
                    "%OH": "%H",
                    "%OI": "%I",
                    "%Om": "%m",
                    "%OM": "%M",
                    "%OS": "%S",
                    "%Ou": "%u",
                    "%OU": "%U",
                    "%OV": "%V",
                    "%Ow": "%w",
                    "%OW": "%W",
                    "%Oy": "%y"
                };
                for (var rule in EXPANSION_RULES_1) {
                    pattern = pattern.replace(new RegExp(rule, "g"), EXPANSION_RULES_1[rule])
                }
                var WEEKDAYS = ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"];
                var MONTHS = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];

                function leadingSomething(value, digits, character) {
                    var str = typeof value === "number" ? value.toString() : value || "";
                    while (str.length < digits) {
                        str = character[0] + str
                    }
                    return str
                }

                function leadingNulls(value, digits) {
                    return leadingSomething(value, digits, "0")
                }

                function compareByDay(date1, date2) {
                    function sgn(value) {
                        return value < 0 ? -1 : value > 0 ? 1 : 0
                    }
                    var compare;
                    if ((compare = sgn(date1.getFullYear() - date2.getFullYear())) === 0) {
                        if ((compare = sgn(date1.getMonth() - date2.getMonth())) === 0) {
                            compare = sgn(date1.getDate() - date2.getDate())
                        }
                    }
                    return compare
                }

                function getFirstWeekStartDate(janFourth) {
                    switch (janFourth.getDay()) {
                        case 0:
                            return new Date(janFourth.getFullYear() - 1, 11, 29);
                        case 1:
                            return janFourth;
                        case 2:
                            return new Date(janFourth.getFullYear(), 0, 3);
                        case 3:
                            return new Date(janFourth.getFullYear(), 0, 2);
                        case 4:
                            return new Date(janFourth.getFullYear(), 0, 1);
                        case 5:
                            return new Date(janFourth.getFullYear() - 1, 11, 31);
                        case 6:
                            return new Date(janFourth.getFullYear() - 1, 11, 30)
                    }
                }

                function getWeekBasedYear(date) {
                    var thisDate = __addDays(new Date(date.tm_year + 1900, 0, 1), date.tm_yday);
                    var janFourthThisYear = new Date(thisDate.getFullYear(), 0, 4);
                    var janFourthNextYear = new Date(thisDate.getFullYear() + 1, 0, 4);
                    var firstWeekStartThisYear = getFirstWeekStartDate(janFourthThisYear);
                    var firstWeekStartNextYear = getFirstWeekStartDate(janFourthNextYear);
                    if (compareByDay(firstWeekStartThisYear, thisDate) <= 0) {
                        if (compareByDay(firstWeekStartNextYear, thisDate) <= 0) {
                            return thisDate.getFullYear() + 1
                        } else {
                            return thisDate.getFullYear()
                        }
                    } else {
                        return thisDate.getFullYear() - 1
                    }
                }
                var EXPANSION_RULES_2 = {
                    "%a": function(date) {
                        return WEEKDAYS[date.tm_wday].substring(0, 3)
                    },
                    "%A": function(date) {
                        return WEEKDAYS[date.tm_wday]
                    },
                    "%b": function(date) {
                        return MONTHS[date.tm_mon].substring(0, 3)
                    },
                    "%B": function(date) {
                        return MONTHS[date.tm_mon]
                    },
                    "%C": function(date) {
                        var year = date.tm_year + 1900;
                        return leadingNulls(year / 100 | 0, 2)
                    },
                    "%d": function(date) {
                        return leadingNulls(date.tm_mday, 2)
                    },
                    "%e": function(date) {
                        return leadingSomething(date.tm_mday, 2, " ")
                    },
                    "%g": function(date) {
                        return getWeekBasedYear(date).toString().substring(2)
                    },
                    "%G": function(date) {
                        return getWeekBasedYear(date)
                    },
                    "%H": function(date) {
                        return leadingNulls(date.tm_hour, 2)
                    },
                    "%I": function(date) {
                        var twelveHour = date.tm_hour;
                        if (twelveHour == 0) twelveHour = 12;
                        else if (twelveHour > 12) twelveHour -= 12;
                        return leadingNulls(twelveHour, 2)
                    },
                    "%j": function(date) {
                        return leadingNulls(date.tm_mday + __arraySum(__isLeapYear(date.tm_year + 1900) ? __MONTH_DAYS_LEAP : __MONTH_DAYS_REGULAR, date.tm_mon - 1), 3)
                    },
                    "%m": function(date) {
                        return leadingNulls(date.tm_mon + 1, 2)
                    },
                    "%M": function(date) {
                        return leadingNulls(date.tm_min, 2)
                    },
                    "%n": function() {
                        return "\n"
                    },
                    "%p": function(date) {
                        if (date.tm_hour >= 0 && date.tm_hour < 12) {
                            return "AM"
                        } else {
                            return "PM"
                        }
                    },
                    "%S": function(date) {
                        return leadingNulls(date.tm_sec, 2)
                    },
                    "%t": function() {
                        return "\t"
                    },
                    "%u": function(date) {
                        return date.tm_wday || 7
                    },
                    "%U": function(date) {
                        var janFirst = new Date(date.tm_year + 1900, 0, 1);
                        var firstSunday = janFirst.getDay() === 0 ? janFirst : __addDays(janFirst, 7 - janFirst.getDay());
                        var endDate = new Date(date.tm_year + 1900, date.tm_mon, date.tm_mday);
                        if (compareByDay(firstSunday, endDate) < 0) {
                            var februaryFirstUntilEndMonth = __arraySum(__isLeapYear(endDate.getFullYear()) ? __MONTH_DAYS_LEAP : __MONTH_DAYS_REGULAR, endDate.getMonth() - 1) - 31;
                            var firstSundayUntilEndJanuary = 31 - firstSunday.getDate();
                            var days = firstSundayUntilEndJanuary + februaryFirstUntilEndMonth + endDate.getDate();
                            return leadingNulls(Math.ceil(days / 7), 2)
                        }
                        return compareByDay(firstSunday, janFirst) === 0 ? "01" : "00"
                    },
                    "%V": function(date) {
                        var janFourthThisYear = new Date(date.tm_year + 1900, 0, 4);
                        var janFourthNextYear = new Date(date.tm_year + 1901, 0, 4);
                        var firstWeekStartThisYear = getFirstWeekStartDate(janFourthThisYear);
                        var firstWeekStartNextYear = getFirstWeekStartDate(janFourthNextYear);
                        var endDate = __addDays(new Date(date.tm_year + 1900, 0, 1), date.tm_yday);
                        if (compareByDay(endDate, firstWeekStartThisYear) < 0) {
                            return "53"
                        }
                        if (compareByDay(firstWeekStartNextYear, endDate) <= 0) {
                            return "01"
                        }
                        var daysDifference;
                        if (firstWeekStartThisYear.getFullYear() < date.tm_year + 1900) {
                            daysDifference = date.tm_yday + 32 - firstWeekStartThisYear.getDate()
                        } else {
                            daysDifference = date.tm_yday + 1 - firstWeekStartThisYear.getDate()
                        }
                        return leadingNulls(Math.ceil(daysDifference / 7), 2)
                    },
                    "%w": function(date) {
                        return date.tm_wday
                    },
                    "%W": function(date) {
                        var janFirst = new Date(date.tm_year, 0, 1);
                        var firstMonday = janFirst.getDay() === 1 ? janFirst : __addDays(janFirst, janFirst.getDay() === 0 ? 1 : 7 - janFirst.getDay() + 1);
                        var endDate = new Date(date.tm_year + 1900, date.tm_mon, date.tm_mday);
                        if (compareByDay(firstMonday, endDate) < 0) {
                            var februaryFirstUntilEndMonth = __arraySum(__isLeapYear(endDate.getFullYear()) ? __MONTH_DAYS_LEAP : __MONTH_DAYS_REGULAR, endDate.getMonth() - 1) - 31;
                            var firstMondayUntilEndJanuary = 31 - firstMonday.getDate();
                            var days = firstMondayUntilEndJanuary + februaryFirstUntilEndMonth + endDate.getDate();
                            return leadingNulls(Math.ceil(days / 7), 2)
                        }
                        return compareByDay(firstMonday, janFirst) === 0 ? "01" : "00"
                    },
                    "%y": function(date) {
                        return (date.tm_year + 1900).toString().substring(2)
                    },
                    "%Y": function(date) {
                        return date.tm_year + 1900
                    },
                    "%z": function(date) {
                        var off = date.tm_gmtoff;
                        var ahead = off >= 0;
                        off = Math.abs(off) / 60;
                        off = off / 60 * 100 + off % 60;
                        return (ahead ? "+" : "-") + String("0000" + off).slice(-4)
                    },
                    "%Z": function(date) {
                        return date.tm_zone
                    },
                    "%%": function() {
                        return "%"
                    }
                };
                pattern = pattern.replace(/%%/g, "\0\0");
                for (var rule in EXPANSION_RULES_2) {
                    if (pattern.includes(rule)) {
                        pattern = pattern.replace(new RegExp(rule, "g"), EXPANSION_RULES_2[rule](date))
                    }
                }
                pattern = pattern.replace(/\0\0/g, "%");
                var bytes = intArrayFromString(pattern, false);
                if (bytes.length > maxsize) {
                    return 0
                }
                writeArrayToMemory(bytes, s);
                return bytes.length - 1
            }

            function _time(ptr) {
                var ret = Date.now() / 1e3 | 0;
                if (ptr) {
                    HEAP32[ptr >> 2] = ret
                }
                return ret
            }
            var FSNode = function(parent, name, mode, rdev) {
                if (!parent) {
                    parent = this
                }
                this.parent = parent;
                this.mount = parent.mount;
                this.mounted = null;
                this.id = FS.nextInode++;
                this.name = name;
                this.mode = mode;
                this.node_ops = {};
                this.stream_ops = {};
                this.rdev = rdev
            };
            var readMode = 292 | 73;
            var writeMode = 146;
            Object.defineProperties(FSNode.prototype, {
                read: {
                    get: function() {
                        return (this.mode & readMode) === readMode
                    },
                    set: function(val) {
                        val ? this.mode |= readMode : this.mode &= ~readMode
                    }
                },
                write: {
                    get: function() {
                        return (this.mode & writeMode) === writeMode
                    },
                    set: function(val) {
                        val ? this.mode |= writeMode : this.mode &= ~writeMode
                    }
                },
                isFolder: {
                    get: function() {
                        return FS.isDir(this.mode)
                    }
                },
                isDevice: {
                    get: function() {
                        return FS.isChrdev(this.mode)
                    }
                }
            });
            FS.FSNode = FSNode;
            FS.staticInit();
            Module["FS_createPath"] = FS.createPath;
            Module["FS_createDataFile"] = FS.createDataFile;
            Module["FS_createPreloadedFile"] = FS.createPreloadedFile;
            Module["FS_createLazyFile"] = FS.createLazyFile;
            Module["FS_createDevice"] = FS.createDevice;
            Module["FS_unlink"] = FS.unlink;
            ERRNO_CODES = {
                "EPERM": 63,
                "ENOENT": 44,
                "ESRCH": 71,
                "EINTR": 27,
                "EIO": 29,
                "ENXIO": 60,
                "E2BIG": 1,
                "ENOEXEC": 45,
                "EBADF": 8,
                "ECHILD": 12,
                "EAGAIN": 6,
                "EWOULDBLOCK": 6,
                "ENOMEM": 48,
                "EACCES": 2,
                "EFAULT": 21,
                "ENOTBLK": 105,
                "EBUSY": 10,
                "EEXIST": 20,
                "EXDEV": 75,
                "ENODEV": 43,
                "ENOTDIR": 54,
                "EISDIR": 31,
                "EINVAL": 28,
                "ENFILE": 41,
                "EMFILE": 33,
                "ENOTTY": 59,
                "ETXTBSY": 74,
                "EFBIG": 22,
                "ENOSPC": 51,
                "ESPIPE": 70,
                "EROFS": 69,
                "EMLINK": 34,
                "EPIPE": 64,
                "EDOM": 18,
                "ERANGE": 68,
                "ENOMSG": 49,
                "EIDRM": 24,
                "ECHRNG": 106,
                "EL2NSYNC": 156,
                "EL3HLT": 107,
                "EL3RST": 108,
                "ELNRNG": 109,
                "EUNATCH": 110,
                "ENOCSI": 111,
                "EL2HLT": 112,
                "EDEADLK": 16,
                "ENOLCK": 46,
                "EBADE": 113,
                "EBADR": 114,
                "EXFULL": 115,
                "ENOANO": 104,
                "EBADRQC": 103,
                "EBADSLT": 102,
                "EDEADLOCK": 16,
                "EBFONT": 101,
                "ENOSTR": 100,
                "ENODATA": 116,
                "ETIME": 117,
                "ENOSR": 118,
                "ENONET": 119,
                "ENOPKG": 120,
                "EREMOTE": 121,
                "ENOLINK": 47,
                "EADV": 122,
                "ESRMNT": 123,
                "ECOMM": 124,
                "EPROTO": 65,
                "EMULTIHOP": 36,
                "EDOTDOT": 125,
                "EBADMSG": 9,
                "ENOTUNIQ": 126,
                "EBADFD": 127,
                "EREMCHG": 128,
                "ELIBACC": 129,
                "ELIBBAD": 130,
                "ELIBSCN": 131,
                "ELIBMAX": 132,
                "ELIBEXEC": 133,
                "ENOSYS": 52,
                "ENOTEMPTY": 55,
                "ENAMETOOLONG": 37,
                "ELOOP": 32,
                "EOPNOTSUPP": 138,
                "EPFNOSUPPORT": 139,
                "ECONNRESET": 15,
                "ENOBUFS": 42,
                "EAFNOSUPPORT": 5,
                "EPROTOTYPE": 67,
                "ENOTSOCK": 57,
                "ENOPROTOOPT": 50,
                "ESHUTDOWN": 140,
                "ECONNREFUSED": 14,
                "EADDRINUSE": 3,
                "ECONNABORTED": 13,
                "ENETUNREACH": 40,
                "ENETDOWN": 38,
                "ETIMEDOUT": 73,
                "EHOSTDOWN": 142,
                "EHOSTUNREACH": 23,
                "EINPROGRESS": 26,
                "EALREADY": 7,
                "EDESTADDRREQ": 17,
                "EMSGSIZE": 35,
                "EPROTONOSUPPORT": 66,
                "ESOCKTNOSUPPORT": 137,
                "EADDRNOTAVAIL": 4,
                "ENETRESET": 39,
                "EISCONN": 30,
                "ENOTCONN": 53,
                "ETOOMANYREFS": 141,
                "EUSERS": 136,
                "EDQUOT": 19,
                "ESTALE": 72,
                "ENOTSUP": 138,
                "ENOMEDIUM": 148,
                "EILSEQ": 25,
                "EOVERFLOW": 61,
                "ECANCELED": 11,
                "ENOTRECOVERABLE": 56,
                "EOWNERDEAD": 62,
                "ESTRPIPE": 135
            };
            var ASSERTIONS = true;

            function intArrayFromString(stringy, dontAddNull, length) {
                var len = length > 0 ? length : lengthBytesUTF8(stringy) + 1;
                var u8array = new Array(len);
                var numBytesWritten = stringToUTF8Array(stringy, u8array, 0, u8array.length);
                if (dontAddNull) u8array.length = numBytesWritten;
                return u8array
            }
            var decodeBase64 = typeof atob === "function" ? atob : function(input) {
                var keyStr = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=";
                var output = "";
                var chr1, chr2, chr3;
                var enc1, enc2, enc3, enc4;
                var i = 0;
                input = input.replace(/[^A-Za-z0-9\+\/\=]/g, "");
                do {
                    enc1 = keyStr.indexOf(input.charAt(i++));
                    enc2 = keyStr.indexOf(input.charAt(i++));
                    enc3 = keyStr.indexOf(input.charAt(i++));
                    enc4 = keyStr.indexOf(input.charAt(i++));
                    chr1 = enc1 << 2 | enc2 >> 4;
                    chr2 = (enc2 & 15) << 4 | enc3 >> 2;
                    chr3 = (enc3 & 3) << 6 | enc4;
                    output = output + String.fromCharCode(chr1);
                    if (enc3 !== 64) {
                        output = output + String.fromCharCode(chr2)
                    }
                    if (enc4 !== 64) {
                        output = output + String.fromCharCode(chr3)
                    }
                } while (i < input.length);
                return output
            };

            function intArrayFromBase64(s) {
                if (typeof ENVIRONMENT_IS_NODE === "boolean" && ENVIRONMENT_IS_NODE) {
                    var buf = Buffer.from(s, "base64");
                    return new Uint8Array(buf["buffer"], buf["byteOffset"], buf["byteLength"])
                }
                try {
                    var decoded = decodeBase64(s);
                    var bytes = new Uint8Array(decoded.length);
                    for (var i = 0; i < decoded.length; ++i) {
                        bytes[i] = decoded.charCodeAt(i)
                    }
                    return bytes
                } catch (_) {
                    throw new Error("Converting base64 string to bytes failed.")
                }
            }
            var asmLibraryArg = {
                "__syscall_access": ___syscall_access,
                "__syscall_chdir": ___syscall_chdir,
                "__syscall_chmod": ___syscall_chmod,
                "__syscall_dup3": ___syscall_dup3,
                "__syscall_fcntl64": ___syscall_fcntl64,
                "__syscall_fstat64": ___syscall_fstat64,
                "__syscall_fstatat64": ___syscall_fstatat64,
                "__syscall_ftruncate64": ___syscall_ftruncate64,
                "__syscall_getcwd": ___syscall_getcwd,
                "__syscall_getdents64": ___syscall_getdents64,
                "__syscall_getuid32": ___syscall_getuid32,
                "__syscall_ioctl": ___syscall_ioctl,
                "__syscall_lstat64": ___syscall_lstat64,
                "__syscall_mkdir": ___syscall_mkdir,
                "__syscall_mmap2": ___syscall_mmap2,
                "__syscall_munmap": ___syscall_munmap,
                "__syscall_open": ___syscall_open,
                "__syscall_poll": ___syscall_poll,
                "__syscall_readlink": ___syscall_readlink,
                "__syscall_rename": ___syscall_rename,
                "__syscall_rmdir": ___syscall_rmdir,
                "__syscall_stat64": ___syscall_stat64,
                "__syscall_unlink": ___syscall_unlink,
                "_dlopen_js": __dlopen_js,
                "_dlsym_js": __dlsym_js,
                "_emscripten_throw_longjmp": __emscripten_throw_longjmp,
                "_localtime_js": __localtime_js,
                "_mktime_js": __mktime_js,
                "_tzset_js": __tzset_js,
                "abort": _abort,
                "clock_gettime": _clock_gettime,
                "emscripten_console_error": _emscripten_console_error,
                "emscripten_get_heap_max": _emscripten_get_heap_max,
                "emscripten_get_now": _emscripten_get_now,
                "emscripten_memcpy_big": _emscripten_memcpy_big,
                "emscripten_resize_heap": _emscripten_resize_heap,
                "environ_get": _environ_get,
                "environ_sizes_get": _environ_sizes_get,
                "exit": _exit,
                "fd_close": _fd_close,
                "fd_fdstat_get": _fd_fdstat_get,
                "fd_read": _fd_read,
                "fd_seek": _fd_seek,
                "fd_write": _fd_write,
                "getTempRet0": _getTempRet0,
                "gettimeofday": _gettimeofday,
                "invoke_i": invoke_i,
                "invoke_ii": invoke_ii,
                "invoke_iii": invoke_iii,
                "invoke_iiii": invoke_iiii,
                "invoke_iiiii": invoke_iiiii,
                "invoke_iiiiii": invoke_iiiiii,
                "invoke_iiiiiii": invoke_iiiiiii,
                "invoke_iiiiiiii": invoke_iiiiiiii,
                "invoke_iiiiiiiii": invoke_iiiiiiiii,
                "invoke_iiiiiiiiii": invoke_iiiiiiiiii,
                "invoke_iiiiiiiiiii": invoke_iiiiiiiiiii,
                "invoke_iiiiiiiiiiii": invoke_iiiiiiiiiiii,
                "invoke_iij": invoke_iij,
                "invoke_iiji": invoke_iiji,
                "invoke_ij": invoke_ij,
                "invoke_v": invoke_v,
                "invoke_vi": invoke_vi,
                "invoke_vii": invoke_vii,
                "invoke_viii": invoke_viii,
                "setTempRet0": _setTempRet0,
                "strftime": _strftime,
                "time": _time
            };
            var asm = createWasm();
            var ___wasm_call_ctors = Module["___wasm_call_ctors"] = createExportWrapper("__wasm_call_ctors");
            var _malloc = Module["_malloc"] = createExportWrapper("malloc");
            var _PL_initialise = Module["_PL_initialise"] = createExportWrapper("PL_initialise");
            var _PL_halt = Module["_PL_halt"] = createExportWrapper("PL_halt");
            var _PL_toplevel = Module["_PL_toplevel"] = createExportWrapper("PL_toplevel");
            var _PL_unregister_blob_type = Module["_PL_unregister_blob_type"] = createExportWrapper("PL_unregister_blob_type");
            var _PL_unregister_atom = Module["_PL_unregister_atom"] = createExportWrapper("PL_unregister_atom");
            var _PL_agc_hook = Module["_PL_agc_hook"] = createExportWrapper("PL_agc_hook");
            var _PL_register_atom = Module["_PL_register_atom"] = createExportWrapper("PL_register_atom");
            var _PL_open_foreign_frame = Module["_PL_open_foreign_frame"] = createExportWrapper("PL_open_foreign_frame");
            var _PL_close_foreign_frame = Module["_PL_close_foreign_frame"] = createExportWrapper("PL_close_foreign_frame");
            var _PL_rewind_foreign_frame = Module["_PL_rewind_foreign_frame"] = createExportWrapper("PL_rewind_foreign_frame");
            var _PL_discard_foreign_frame = Module["_PL_discard_foreign_frame"] = createExportWrapper("PL_discard_foreign_frame");
            var _PL_open_query = Module["_PL_open_query"] = createExportWrapper("PL_open_query");
            var _PL_exception = Module["_PL_exception"] = createExportWrapper("PL_exception");
            var _PL_cut_query = Module["_PL_cut_query"] = createExportWrapper("PL_cut_query");
            var _PL_close_query = Module["_PL_close_query"] = createExportWrapper("PL_close_query");
            var _PL_current_query = Module["_PL_current_query"] = createExportWrapper("PL_current_query");
            var _PL_next_solution = Module["_PL_next_solution"] = createExportWrapper("PL_next_solution");
            var _PL_instantiation_error = Module["_PL_instantiation_error"] = createExportWrapper("PL_instantiation_error");
            var _PL_uninstantiation_error = Module["_PL_uninstantiation_error"] = createExportWrapper("PL_uninstantiation_error");
            var _PL_representation_error = Module["_PL_representation_error"] = createExportWrapper("PL_representation_error");
            var _PL_type_error = Module["_PL_type_error"] = createExportWrapper("PL_type_error");
            var _PL_domain_error = Module["_PL_domain_error"] = createExportWrapper("PL_domain_error");
            var _PL_existence_error = Module["_PL_existence_error"] = createExportWrapper("PL_existence_error");
            var _PL_permission_error = Module["_PL_permission_error"] = createExportWrapper("PL_permission_error");
            var _PL_resource_error = Module["_PL_resource_error"] = createExportWrapper("PL_resource_error");
            var _PL_syntax_error = Module["_PL_syntax_error"] = createExportWrapper("PL_syntax_error");
            var _PL_get_atom_ex = Module["_PL_get_atom_ex"] = createExportWrapper("PL_get_atom_ex");
            var _PL_get_integer_ex = Module["_PL_get_integer_ex"] = createExportWrapper("PL_get_integer_ex");
            var _PL_get_long_ex = Module["_PL_get_long_ex"] = createExportWrapper("PL_get_long_ex");
            var _PL_get_int64_ex = Module["_PL_get_int64_ex"] = createExportWrapper("PL_get_int64_ex");
            var _PL_get_intptr_ex = Module["_PL_get_intptr_ex"] = createExportWrapper("PL_get_intptr_ex");
            var _PL_get_size_ex = Module["_PL_get_size_ex"] = createExportWrapper("PL_get_size_ex");
            var _PL_get_bool_ex = Module["_PL_get_bool_ex"] = createExportWrapper("PL_get_bool_ex");
            var _PL_get_float_ex = Module["_PL_get_float_ex"] = createExportWrapper("PL_get_float_ex");
            var _PL_get_char_ex = Module["_PL_get_char_ex"] = createExportWrapper("PL_get_char_ex");
            var _PL_get_pointer_ex = Module["_PL_get_pointer_ex"] = createExportWrapper("PL_get_pointer_ex");
            var _PL_unify_list_ex = Module["_PL_unify_list_ex"] = createExportWrapper("PL_unify_list_ex");
            var _PL_unify_nil_ex = Module["_PL_unify_nil_ex"] = createExportWrapper("PL_unify_nil_ex");
            var _PL_get_list_ex = Module["_PL_get_list_ex"] = createExportWrapper("PL_get_list_ex");
            var _PL_get_nil_ex = Module["_PL_get_nil_ex"] = createExportWrapper("PL_get_nil_ex");
            var _PL_unify_bool_ex = Module["_PL_unify_bool_ex"] = createExportWrapper("PL_unify_bool_ex");
            var _PL_is_ground = Module["_PL_is_ground"] = createExportWrapper("PL_is_ground");
            var _PL_is_acyclic = Module["_PL_is_acyclic"] = createExportWrapper("PL_is_acyclic");
            var _PL_chars_to_term = Module["_PL_chars_to_term"] = createExportWrapper("PL_chars_to_term");
            var _PL_wchars_to_term = Module["_PL_wchars_to_term"] = createExportWrapper("PL_wchars_to_term");
            var _PL_record_external = Module["_PL_record_external"] = createExportWrapper("PL_record_external");
            var _PL_recorded_external = Module["_PL_recorded_external"] = createExportWrapper("PL_recorded_external");
            var _PL_erase_external = Module["_PL_erase_external"] = createExportWrapper("PL_erase_external");
            var _PL_sigaction = Module["_PL_sigaction"] = createExportWrapper("PL_sigaction");
            var _PL_get_signum_ex = Module["_PL_get_signum_ex"] = createExportWrapper("PL_get_signum_ex");
            var _PL_signal = Module["_PL_signal"] = createExportWrapper("PL_signal");
            var _PL_handle_signals = Module["_PL_handle_signals"] = createExportWrapper("PL_handle_signals");
            var _PL_cleanup_fork = Module["_PL_cleanup_fork"] = createExportWrapper("PL_cleanup_fork");
            var _PL_is_initialised = Module["_PL_is_initialised"] = createExportWrapper("PL_is_initialised");
            var _free = Module["_free"] = createExportWrapper("free");
            var _PL_raise = Module["_PL_raise"] = createExportWrapper("PL_raise");
            var _PL_new_atom = Module["_PL_new_atom"] = createExportWrapper("PL_new_atom");
            var ___errno_location = Module["___errno_location"] = createExportWrapper("__errno_location");
            var _PL_put_atom_chars = Module["_PL_put_atom_chars"] = createExportWrapper("PL_put_atom_chars");
            var _PL_throw = Module["_PL_throw"] = createExportWrapper("PL_throw");
            var _PL_raise_exception = Module["_PL_raise_exception"] = createExportWrapper("PL_raise_exception");
            var _PL_clear_exception = Module["_PL_clear_exception"] = createExportWrapper("PL_clear_exception");
            var _PL_put_nil = Module["_PL_put_nil"] = createExportWrapper("PL_put_nil");
            var _PL_atom_nchars = Module["_PL_atom_nchars"] = createExportWrapper("PL_atom_nchars");
            var _PL_atom_wchars = Module["_PL_atom_wchars"] = createExportWrapper("PL_atom_wchars");
            var _PL_unify_nil = Module["_PL_unify_nil"] = createExportWrapper("PL_unify_nil");
            var _PL_cons_functor_v = Module["_PL_cons_functor_v"] = createExportWrapper("PL_cons_functor_v");
            var _PL_get_nil = Module["_PL_get_nil"] = createExportWrapper("PL_get_nil");
            var _PL_atom_chars = Module["_PL_atom_chars"] = createExportWrapper("PL_atom_chars");
            var _PL_is_list = Module["_PL_is_list"] = createExportWrapper("PL_is_list");
            var _PL_cons_functor = Module["_PL_cons_functor"] = createExportWrapper("PL_cons_functor");
            var _PL_warning = Module["_PL_warning"] = createExportWrapper("PL_warning");
            var _PL_is_integer = Module["_PL_is_integer"] = createExportWrapper("PL_is_integer");
            var _PL_unify_chars = Module["_PL_unify_chars"] = createExportWrapper("PL_unify_chars");
            var _PL_unify_float = Module["_PL_unify_float"] = createExportWrapper("PL_unify_float");
            var _PL_get_nchars = Module["_PL_get_nchars"] = createExportWrapper("PL_get_nchars");
            var _PL_get_wchars = Module["_PL_get_wchars"] = createExportWrapper("PL_get_wchars");
            var _PL_call_predicate = Module["_PL_call_predicate"] = createExportWrapper("PL_call_predicate");
            var _PL_is_number = Module["_PL_is_number"] = createExportWrapper("PL_is_number");
            var _PL_is_string = Module["_PL_is_string"] = createExportWrapper("PL_is_string");
            var _PL_is_pair = Module["_PL_is_pair"] = createExportWrapper("PL_is_pair");
            var _PL_predicate = Module["_PL_predicate"] = createExportWrapper("PL_predicate");
            var _PL_is_float = Module["_PL_is_float"] = createExportWrapper("PL_is_float");
            var _PL_is_compound = Module["_PL_is_compound"] = createExportWrapper("PL_is_compound");
            var _PL_is_callable = Module["_PL_is_callable"] = createExportWrapper("PL_is_callable");
            var _PL_unify_compound = Module["_PL_unify_compound"] = createExportWrapper("PL_unify_compound");
            var _PL_compare = Module["_PL_compare"] = createExportWrapper("PL_compare");
            var _PL_unify_uint64 = Module["_PL_unify_uint64"] = createExportWrapper("PL_unify_uint64");
            var _PL_unify_atom_nchars = Module["_PL_unify_atom_nchars"] = createExportWrapper("PL_unify_atom_nchars");
            var _PL_unify_wchars = Module["_PL_unify_wchars"] = createExportWrapper("PL_unify_wchars");
            var _PL_get_atom_chars = Module["_PL_get_atom_chars"] = createExportWrapper("PL_get_atom_chars");
            var _PL_unify_bool = Module["_PL_unify_bool"] = createExportWrapper("PL_unify_bool");
            var _PL_get_chars = Module["_PL_get_chars"] = createExportWrapper("PL_get_chars");
            var _PL_skip_list = Module["_PL_skip_list"] = createExportWrapper("PL_skip_list");
            var _PL_is_atom = Module["_PL_is_atom"] = createExportWrapper("PL_is_atom");
            var _PL_is_variable = Module["_PL_is_variable"] = createExportWrapper("PL_is_variable");
            var _PL_unify_atom = Module["_PL_unify_atom"] = createExportWrapper("PL_unify_atom");
            var _PL_new_term_refs = Module["_PL_new_term_refs"] = createExportWrapper("PL_new_term_refs");
            var _PL_put_atom = Module["_PL_put_atom"] = createExportWrapper("PL_put_atom");
            var _PL_new_term_ref = Module["_PL_new_term_ref"] = createExportWrapper("PL_new_term_ref");
            var _PL_unify = Module["_PL_unify"] = createExportWrapper("PL_unify");
            var _PL_get_bool = Module["_PL_get_bool"] = createExportWrapper("PL_get_bool");
            var _PL_get_float = Module["_PL_get_float"] = createExportWrapper("PL_get_float");
            var _PL_get_module = Module["_PL_get_module"] = createExportWrapper("PL_get_module");
            var _PL_erase = Module["_PL_erase"] = createExportWrapper("PL_erase");
            var _PL_unify_string_nchars = Module["_PL_unify_string_nchars"] = createExportWrapper("PL_unify_string_nchars");
            var _PL_get_intptr = Module["_PL_get_intptr"] = createExportWrapper("PL_get_intptr");
            var _PL_pred = Module["_PL_pred"] = createExportWrapper("PL_pred");
            var _PL_is_blob = Module["_PL_is_blob"] = createExportWrapper("PL_is_blob");
            var _saveSetjmp = Module["_saveSetjmp"] = createExportWrapper("saveSetjmp");
            var _PL_unify_atom_chars = Module["_PL_unify_atom_chars"] = createExportWrapper("PL_unify_atom_chars");
            var _PL_put_float = Module["_PL_put_float"] = createExportWrapper("PL_put_float");
            var _PL_put_pointer = Module["_PL_put_pointer"] = createExportWrapper("PL_put_pointer");
            var _PL_unify_int64 = Module["_PL_unify_int64"] = createExportWrapper("PL_unify_int64");
            var _PL_get_atom = Module["_PL_get_atom"] = createExportWrapper("PL_get_atom");
            var _PL_copy_term_ref = Module["_PL_copy_term_ref"] = createExportWrapper("PL_copy_term_ref");
            var _PL_unify_integer = Module["_PL_unify_integer"] = createExportWrapper("PL_unify_integer");
            var _PL_put_int64 = Module["_PL_put_int64"] = createExportWrapper("PL_put_int64");
            var _PL_set_prolog_flag = Module["_PL_set_prolog_flag"] = createExportWrapper("PL_set_prolog_flag");
            var _PL_get_file_name = Module["_PL_get_file_name"] = createExportWrapper("PL_get_file_name");
            var _PL_unify_blob = Module["_PL_unify_blob"] = createExportWrapper("PL_unify_blob");
            var _PL_get_blob = Module["_PL_get_blob"] = createExportWrapper("PL_get_blob");
            var _PL_blob_data = Module["_PL_blob_data"] = createExportWrapper("PL_blob_data");
            var _PL_new_module = Module["_PL_new_module"] = createExportWrapper("PL_new_module");
            var _PL_put_string_chars = Module["_PL_put_string_chars"] = createExportWrapper("PL_put_string_chars");
            var _PL_set_resource_db_mem = Module["_PL_set_resource_db_mem"] = createExportWrapper("PL_set_resource_db_mem");
            var _PL_on_halt = Module["_PL_on_halt"] = createExportWrapper("PL_on_halt");
            var _PL_exit_hook = Module["_PL_exit_hook"] = createExportWrapper("PL_exit_hook");
            var _PL_cleanup = Module["_PL_cleanup"] = createExportWrapper("PL_cleanup");
            var _PL_unify_string_chars = Module["_PL_unify_string_chars"] = createExportWrapper("PL_unify_string_chars");
            var _PL_put_variable = Module["_PL_put_variable"] = createExportWrapper("PL_put_variable");
            var _PL_is_atomic = Module["_PL_is_atomic"] = createExportWrapper("PL_is_atomic");
            var _PL_recorded = Module["_PL_recorded"] = createExportWrapper("PL_recorded");
            var _PL_record = Module["_PL_record"] = createExportWrapper("PL_record");
            var _PL_put_functor = Module["_PL_put_functor"] = createExportWrapper("PL_put_functor");
            var _PL_strip_module = Module["_PL_strip_module"] = createExportWrapper("PL_strip_module");
            var _PL_unify_list = Module["_PL_unify_list"] = createExportWrapper("PL_unify_list");
            var _PL_cons_list = Module["_PL_cons_list"] = createExportWrapper("PL_cons_list");
            var _PL_register_foreign_in_module = Module["_PL_register_foreign_in_module"] = createExportWrapper("PL_register_foreign_in_module");
            var _PL_foreign_control = Module["_PL_foreign_control"] = createExportWrapper("PL_foreign_control");
            var _PL_foreign_context_address = Module["_PL_foreign_context_address"] = createExportWrapper("PL_foreign_context_address");
            var _PL_reset_term_refs = Module["_PL_reset_term_refs"] = createExportWrapper("PL_reset_term_refs");
            var _PL_new_atom_nchars = Module["_PL_new_atom_nchars"] = createExportWrapper("PL_new_atom_nchars");
            var _PL_new_atom_mbchars = Module["_PL_new_atom_mbchars"] = createExportWrapper("PL_new_atom_mbchars");
            var _PL_new_functor = Module["_PL_new_functor"] = createExportWrapper("PL_new_functor");
            var _PL_functor_name = Module["_PL_functor_name"] = createExportWrapper("PL_functor_name");
            var _PL_functor_arity = Module["_PL_functor_arity"] = createExportWrapper("PL_functor_arity");
            var _PL_new_atom_wchars = Module["_PL_new_atom_wchars"] = createExportWrapper("PL_new_atom_wchars");
            var _PL_unify_wchars_diff = Module["_PL_unify_wchars_diff"] = createExportWrapper("PL_unify_wchars_diff");
            var _PL_same_compound = Module["_PL_same_compound"] = createExportWrapper("PL_same_compound");
            var _PL_get_atom_nchars = Module["_PL_get_atom_nchars"] = createExportWrapper("PL_get_atom_nchars");
            var _PL_get_list_nchars = Module["_PL_get_list_nchars"] = createExportWrapper("PL_get_list_nchars");
            var _PL_get_list_chars = Module["_PL_get_list_chars"] = createExportWrapper("PL_get_list_chars");
            var _PL_quote = Module["_PL_quote"] = createExportWrapper("PL_quote");
            var _PL_get_integer = Module["_PL_get_integer"] = createExportWrapper("PL_get_integer");
            var _PL_get_long = Module["_PL_get_long"] = createExportWrapper("PL_get_long");
            var _PL_get_int64 = Module["_PL_get_int64"] = createExportWrapper("PL_get_int64");
            var _PL_get_pointer = Module["_PL_get_pointer"] = createExportWrapper("PL_get_pointer");
            var _PL_get_name_arity = Module["_PL_get_name_arity"] = createExportWrapper("PL_get_name_arity");
            var _PL_get_compound_name_arity = Module["_PL_get_compound_name_arity"] = createExportWrapper("PL_get_compound_name_arity");
            var _PL_get_functor = Module["_PL_get_functor"] = createExportWrapper("PL_get_functor");
            var _PL_get_arg = Module["_PL_get_arg"] = createExportWrapper("PL_get_arg");
            var _PL_get_list = Module["_PL_get_list"] = createExportWrapper("PL_get_list");
            var _PL_get_head = Module["_PL_get_head"] = createExportWrapper("PL_get_head");
            var _PL_get_tail = Module["_PL_get_tail"] = createExportWrapper("PL_get_tail");
            var _PL_is_functor = Module["_PL_is_functor"] = createExportWrapper("PL_is_functor");
            var _PL_put_bool = Module["_PL_put_bool"] = createExportWrapper("PL_put_bool");
            var _PL_put_atom_nchars = Module["_PL_put_atom_nchars"] = createExportWrapper("PL_put_atom_nchars");
            var _PL_put_string_nchars = Module["_PL_put_string_nchars"] = createExportWrapper("PL_put_string_nchars");
            var _PL_put_chars = Module["_PL_put_chars"] = createExportWrapper("PL_put_chars");
            var _PL_put_list_ncodes = Module["_PL_put_list_ncodes"] = createExportWrapper("PL_put_list_ncodes");
            var _PL_put_list_nchars = Module["_PL_put_list_nchars"] = createExportWrapper("PL_put_list_nchars");
            var _PL_put_list_chars = Module["_PL_put_list_chars"] = createExportWrapper("PL_put_list_chars");
            var _PL_put_integer = Module["_PL_put_integer"] = createExportWrapper("PL_put_integer");
            var _PL_put_list = Module["_PL_put_list"] = createExportWrapper("PL_put_list");
            var _PL_put_term = Module["_PL_put_term"] = createExportWrapper("PL_put_term");
            var _PL_unify_functor = Module["_PL_unify_functor"] = createExportWrapper("PL_unify_functor");
            var _PL_unify_list_ncodes = Module["_PL_unify_list_ncodes"] = createExportWrapper("PL_unify_list_ncodes");
            var _PL_unify_list_nchars = Module["_PL_unify_list_nchars"] = createExportWrapper("PL_unify_list_nchars");
            var _PL_unify_list_chars = Module["_PL_unify_list_chars"] = createExportWrapper("PL_unify_list_chars");
            var _PL_unify_pointer = Module["_PL_unify_pointer"] = createExportWrapper("PL_unify_pointer");
            var _PL_unify_arg = Module["_PL_unify_arg"] = createExportWrapper("PL_unify_arg");
            var _PL_unify_term = Module["_PL_unify_term"] = createExportWrapper("PL_unify_term");
            var _PL_put_blob = Module["_PL_put_blob"] = createExportWrapper("PL_put_blob");
            var _PL_term_type = Module["_PL_term_type"] = createExportWrapper("PL_term_type");
            var _PL_context = Module["_PL_context"] = createExportWrapper("PL_context");
            var _PL_module_name = Module["_PL_module_name"] = createExportWrapper("PL_module_name");
            var _PL_predicate_info = Module["_PL_predicate_info"] = createExportWrapper("PL_predicate_info");
            var _PL_call = Module["_PL_call"] = createExportWrapper("PL_call");
            var _PL_foreign_context = Module["_PL_foreign_context"] = createExportWrapper("PL_foreign_context");
            var _PL_foreign_context_predicate = Module["_PL_foreign_context_predicate"] = createExportWrapper("PL_foreign_context_predicate");
            var _PL_register_extensions_in_module = Module["_PL_register_extensions_in_module"] = createExportWrapper("PL_register_extensions_in_module");
            var _PL_register_extensions = Module["_PL_register_extensions"] = createExportWrapper("PL_register_extensions");
            var _PL_register_foreign = Module["_PL_register_foreign"] = createExportWrapper("PL_register_foreign");
            var _PL_abort_hook = Module["_PL_abort_hook"] = createExportWrapper("PL_abort_hook");
            var _PL_abort_unhook = Module["_PL_abort_unhook"] = createExportWrapper("PL_abort_unhook");
            var _PL_dispatch_hook = Module["_PL_dispatch_hook"] = createExportWrapper("PL_dispatch_hook");
            var _PL_duplicate_record = Module["_PL_duplicate_record"] = createExportWrapper("PL_duplicate_record");
            var _PL_action = Module["_PL_action"] = createExportWrapper("PL_action");
            var _PL_query = Module["_PL_query"] = createExportWrapper("PL_query");
            var _PL_get_file_nameW = Module["_PL_get_file_nameW"] = createExportWrapper("PL_get_file_nameW");
            var ___stdio_exit = Module["___stdio_exit"] = createExportWrapper("__stdio_exit");
            var ___funcs_on_exit = Module["___funcs_on_exit"] = createExportWrapper("__funcs_on_exit");
            var ___dl_seterr = Module["___dl_seterr"] = createExportWrapper("__dl_seterr");
            var _memalign = Module["_memalign"] = createExportWrapper("memalign");
            var _setThrew = Module["_setThrew"] = createExportWrapper("setThrew");
            var _emscripten_stack_init = Module["_emscripten_stack_init"] = function() {
                return (_emscripten_stack_init = Module["_emscripten_stack_init"] = Module["asm"]["emscripten_stack_init"]).apply(null, arguments)
            };
            var _emscripten_stack_get_free = Module["_emscripten_stack_get_free"] = function() {
                return (_emscripten_stack_get_free = Module["_emscripten_stack_get_free"] = Module["asm"]["emscripten_stack_get_free"]).apply(null, arguments)
            };
            var _emscripten_stack_get_base = Module["_emscripten_stack_get_base"] = function() {
                return (_emscripten_stack_get_base = Module["_emscripten_stack_get_base"] = Module["asm"]["emscripten_stack_get_base"]).apply(null, arguments)
            };
            var _emscripten_stack_get_end = Module["_emscripten_stack_get_end"] = function() {
                return (_emscripten_stack_get_end = Module["_emscripten_stack_get_end"] = Module["asm"]["emscripten_stack_get_end"]).apply(null, arguments)
            };
            var stackSave = Module["stackSave"] = createExportWrapper("stackSave");
            var stackRestore = Module["stackRestore"] = createExportWrapper("stackRestore");
            var stackAlloc = Module["stackAlloc"] = createExportWrapper("stackAlloc");
            var dynCall_iiji = Module["dynCall_iiji"] = createExportWrapper("dynCall_iiji");
            var dynCall_iij = Module["dynCall_iij"] = createExportWrapper("dynCall_iij");
            var dynCall_ij = Module["dynCall_ij"] = createExportWrapper("dynCall_ij");
            var dynCall_jii = Module["dynCall_jii"] = createExportWrapper("dynCall_jii");
            var dynCall_iiiji = Module["dynCall_iiiji"] = createExportWrapper("dynCall_iiiji");
            var dynCall_jiji = Module["dynCall_jiji"] = createExportWrapper("dynCall_jiji");

            function invoke_iii(index, a1, a2) {
                var sp = stackSave();
                try {
                    return getWasmTableEntry(index)(a1, a2)
                } catch (e) {
                    stackRestore(sp);
                    if (e !== e + 0 && e !== "longjmp") throw e;
                    _setThrew(1, 0)
                }
            }

            function invoke_vi(index, a1) {
                var sp = stackSave();
                try {
                    getWasmTableEntry(index)(a1)
                } catch (e) {
                    stackRestore(sp);
                    if (e !== e + 0 && e !== "longjmp") throw e;
                    _setThrew(1, 0)
                }
            }

            function invoke_ii(index, a1) {
                var sp = stackSave();
                try {
                    return getWasmTableEntry(index)(a1)
                } catch (e) {
                    stackRestore(sp);
                    if (e !== e + 0 && e !== "longjmp") throw e;
                    _setThrew(1, 0)
                }
            }

            function invoke_viii(index, a1, a2, a3) {
                var sp = stackSave();
                try {
                    getWasmTableEntry(index)(a1, a2, a3)
                } catch (e) {
                    stackRestore(sp);
                    if (e !== e + 0 && e !== "longjmp") throw e;
                    _setThrew(1, 0)
                }
            }

            function invoke_iiii(index, a1, a2, a3) {
                var sp = stackSave();
                try {
                    return getWasmTableEntry(index)(a1, a2, a3)
                } catch (e) {
                    stackRestore(sp);
                    if (e !== e + 0 && e !== "longjmp") throw e;
                    _setThrew(1, 0)
                }
            }

            function invoke_vii(index, a1, a2) {
                var sp = stackSave();
                try {
                    getWasmTableEntry(index)(a1, a2)
                } catch (e) {
                    stackRestore(sp);
                    if (e !== e + 0 && e !== "longjmp") throw e;
                    _setThrew(1, 0)
                }
            }

            function invoke_v(index) {
                var sp = stackSave();
                try {
                    getWasmTableEntry(index)()
                } catch (e) {
                    stackRestore(sp);
                    if (e !== e + 0 && e !== "longjmp") throw e;
                    _setThrew(1, 0)
                }
            }

            function invoke_iiiiii(index, a1, a2, a3, a4, a5) {
                var sp = stackSave();
                try {
                    return getWasmTableEntry(index)(a1, a2, a3, a4, a5)
                } catch (e) {
                    stackRestore(sp);
                    if (e !== e + 0 && e !== "longjmp") throw e;
                    _setThrew(1, 0)
                }
            }

            function invoke_i(index) {
                var sp = stackSave();
                try {
                    return getWasmTableEntry(index)()
                } catch (e) {
                    stackRestore(sp);
                    if (e !== e + 0 && e !== "longjmp") throw e;
                    _setThrew(1, 0)
                }
            }

            function invoke_iiiiiiiiiiii(index, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) {
                var sp = stackSave();
                try {
                    return getWasmTableEntry(index)(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)
                } catch (e) {
                    stackRestore(sp);
                    if (e !== e + 0 && e !== "longjmp") throw e;
                    _setThrew(1, 0)
                }
            }

            function invoke_iiiiiiiiiii(index, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) {
                var sp = stackSave();
                try {
                    return getWasmTableEntry(index)(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
                } catch (e) {
                    stackRestore(sp);
                    if (e !== e + 0 && e !== "longjmp") throw e;
                    _setThrew(1, 0)
                }
            }

            function invoke_iiiiiiiiii(index, a1, a2, a3, a4, a5, a6, a7, a8, a9) {
                var sp = stackSave();
                try {
                    return getWasmTableEntry(index)(a1, a2, a3, a4, a5, a6, a7, a8, a9)
                } catch (e) {
                    stackRestore(sp);
                    if (e !== e + 0 && e !== "longjmp") throw e;
                    _setThrew(1, 0)
                }
            }

            function invoke_iiiiiiiii(index, a1, a2, a3, a4, a5, a6, a7, a8) {
                var sp = stackSave();
                try {
                    return getWasmTableEntry(index)(a1, a2, a3, a4, a5, a6, a7, a8)
                } catch (e) {
                    stackRestore(sp);
                    if (e !== e + 0 && e !== "longjmp") throw e;
                    _setThrew(1, 0)
                }
            }

            function invoke_iiiiiiii(index, a1, a2, a3, a4, a5, a6, a7) {
                var sp = stackSave();
                try {
                    return getWasmTableEntry(index)(a1, a2, a3, a4, a5, a6, a7)
                } catch (e) {
                    stackRestore(sp);
                    if (e !== e + 0 && e !== "longjmp") throw e;
                    _setThrew(1, 0)
                }
            }

            function invoke_iiiiiii(index, a1, a2, a3, a4, a5, a6) {
                var sp = stackSave();
                try {
                    return getWasmTableEntry(index)(a1, a2, a3, a4, a5, a6)
                } catch (e) {
                    stackRestore(sp);
                    if (e !== e + 0 && e !== "longjmp") throw e;
                    _setThrew(1, 0)
                }
            }

            function invoke_iiiii(index, a1, a2, a3, a4) {
                var sp = stackSave();
                try {
                    return getWasmTableEntry(index)(a1, a2, a3, a4)
                } catch (e) {
                    stackRestore(sp);
                    if (e !== e + 0 && e !== "longjmp") throw e;
                    _setThrew(1, 0)
                }
            }

            function invoke_iiji(index, a1, a2, a3, a4) {
                var sp = stackSave();
                try {
                    return dynCall_iiji(index, a1, a2, a3, a4)
                } catch (e) {
                    stackRestore(sp);
                    if (e !== e + 0 && e !== "longjmp") throw e;
                    _setThrew(1, 0)
                }
            }

            function invoke_iij(index, a1, a2, a3) {
                var sp = stackSave();
                try {
                    return dynCall_iij(index, a1, a2, a3)
                } catch (e) {
                    stackRestore(sp);
                    if (e !== e + 0 && e !== "longjmp") throw e;
                    _setThrew(1, 0)
                }
            }

            function invoke_ij(index, a1, a2) {
                var sp = stackSave();
                try {
                    return dynCall_ij(index, a1, a2)
                } catch (e) {
                    stackRestore(sp);
                    if (e !== e + 0 && e !== "longjmp") throw e;
                    _setThrew(1, 0)
                }
            }
            Module["intArrayFromString"] = intArrayFromString;
            if (!Object.getOwnPropertyDescriptor(Module, "intArrayToString")) Module["intArrayToString"] = (() => abort("'intArrayToString' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "ccall")) Module["ccall"] = (() => abort("'ccall' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            Module["cwrap"] = cwrap;
            Module["setValue"] = setValue;
            Module["getValue"] = getValue;
            Module["allocate"] = allocate;
            if (!Object.getOwnPropertyDescriptor(Module, "UTF8ArrayToString")) Module["UTF8ArrayToString"] = (() => abort("'UTF8ArrayToString' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            Module["UTF8ToString"] = UTF8ToString;
            if (!Object.getOwnPropertyDescriptor(Module, "stringToUTF8Array")) Module["stringToUTF8Array"] = (() => abort("'stringToUTF8Array' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            Module["stringToUTF8"] = stringToUTF8;
            Module["lengthBytesUTF8"] = lengthBytesUTF8;
            if (!Object.getOwnPropertyDescriptor(Module, "stackTrace")) Module["stackTrace"] = (() => abort("'stackTrace' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "addOnPreRun")) Module["addOnPreRun"] = (() => abort("'addOnPreRun' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "addOnInit")) Module["addOnInit"] = (() => abort("'addOnInit' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "addOnPreMain")) Module["addOnPreMain"] = (() => abort("'addOnPreMain' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "addOnExit")) Module["addOnExit"] = (() => abort("'addOnExit' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "addOnPostRun")) Module["addOnPostRun"] = (() => abort("'addOnPostRun' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "writeStringToMemory")) Module["writeStringToMemory"] = (() => abort("'writeStringToMemory' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "writeArrayToMemory")) Module["writeArrayToMemory"] = (() => abort("'writeArrayToMemory' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "writeAsciiToMemory")) Module["writeAsciiToMemory"] = (() => abort("'writeAsciiToMemory' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            Module["addRunDependency"] = addRunDependency;
            Module["removeRunDependency"] = removeRunDependency;
            if (!Object.getOwnPropertyDescriptor(Module, "FS_createFolder")) Module["FS_createFolder"] = (() => abort("'FS_createFolder' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            Module["FS_createPath"] = FS.createPath;
            Module["FS_createDataFile"] = FS.createDataFile;
            Module["FS_createPreloadedFile"] = FS.createPreloadedFile;
            Module["FS_createLazyFile"] = FS.createLazyFile;
            if (!Object.getOwnPropertyDescriptor(Module, "FS_createLink")) Module["FS_createLink"] = (() => abort("'FS_createLink' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            Module["FS_createDevice"] = FS.createDevice;
            Module["FS_unlink"] = FS.unlink;
            if (!Object.getOwnPropertyDescriptor(Module, "getLEB")) Module["getLEB"] = (() => abort("'getLEB' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "getFunctionTables")) Module["getFunctionTables"] = (() => abort("'getFunctionTables' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "alignFunctionTables")) Module["alignFunctionTables"] = (() => abort("'alignFunctionTables' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "registerFunctions")) Module["registerFunctions"] = (() => abort("'registerFunctions' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "addFunction")) Module["addFunction"] = (() => abort("'addFunction' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "removeFunction")) Module["removeFunction"] = (() => abort("'removeFunction' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "getFuncWrapper")) Module["getFuncWrapper"] = (() => abort("'getFuncWrapper' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "prettyPrint")) Module["prettyPrint"] = (() => abort("'prettyPrint' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "dynCall")) Module["dynCall"] = (() => abort("'dynCall' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "getCompilerSetting")) Module["getCompilerSetting"] = (() => abort("'getCompilerSetting' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "print")) Module["print"] = (() => abort("'print' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "printErr")) Module["printErr"] = (() => abort("'printErr' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "getTempRet0")) Module["getTempRet0"] = (() => abort("'getTempRet0' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "setTempRet0")) Module["setTempRet0"] = (() => abort("'setTempRet0' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "callMain")) Module["callMain"] = (() => abort("'callMain' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "abort")) Module["abort"] = (() => abort("'abort' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "keepRuntimeAlive")) Module["keepRuntimeAlive"] = (() => abort("'keepRuntimeAlive' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "zeroMemory")) Module["zeroMemory"] = (() => abort("'zeroMemory' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "stringToNewUTF8")) Module["stringToNewUTF8"] = (() => abort("'stringToNewUTF8' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "setFileTime")) Module["setFileTime"] = (() => abort("'setFileTime' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "emscripten_realloc_buffer")) Module["emscripten_realloc_buffer"] = (() => abort("'emscripten_realloc_buffer' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "ENV")) Module["ENV"] = (() => abort("'ENV' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "withStackSave")) Module["withStackSave"] = (() => abort("'withStackSave' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "ERRNO_CODES")) Module["ERRNO_CODES"] = (() => abort("'ERRNO_CODES' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "ERRNO_MESSAGES")) Module["ERRNO_MESSAGES"] = (() => abort("'ERRNO_MESSAGES' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "setErrNo")) Module["setErrNo"] = (() => abort("'setErrNo' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "inetPton4")) Module["inetPton4"] = (() => abort("'inetPton4' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "inetNtop4")) Module["inetNtop4"] = (() => abort("'inetNtop4' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "inetPton6")) Module["inetPton6"] = (() => abort("'inetPton6' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "inetNtop6")) Module["inetNtop6"] = (() => abort("'inetNtop6' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "readSockaddr")) Module["readSockaddr"] = (() => abort("'readSockaddr' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "writeSockaddr")) Module["writeSockaddr"] = (() => abort("'writeSockaddr' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "DNS")) Module["DNS"] = (() => abort("'DNS' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "getHostByName")) Module["getHostByName"] = (() => abort("'getHostByName' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "Protocols")) Module["Protocols"] = (() => abort("'Protocols' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "Sockets")) Module["Sockets"] = (() => abort("'Sockets' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "getRandomDevice")) Module["getRandomDevice"] = (() => abort("'getRandomDevice' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "traverseStack")) Module["traverseStack"] = (() => abort("'traverseStack' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "convertFrameToPC")) Module["convertFrameToPC"] = (() => abort("'convertFrameToPC' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "UNWIND_CACHE")) Module["UNWIND_CACHE"] = (() => abort("'UNWIND_CACHE' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "saveInUnwindCache")) Module["saveInUnwindCache"] = (() => abort("'saveInUnwindCache' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "convertPCtoSourceLocation")) Module["convertPCtoSourceLocation"] = (() => abort("'convertPCtoSourceLocation' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "readAsmConstArgsArray")) Module["readAsmConstArgsArray"] = (() => abort("'readAsmConstArgsArray' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "readAsmConstArgs")) Module["readAsmConstArgs"] = (() => abort("'readAsmConstArgs' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "mainThreadEM_ASM")) Module["mainThreadEM_ASM"] = (() => abort("'mainThreadEM_ASM' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "jstoi_q")) Module["jstoi_q"] = (() => abort("'jstoi_q' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "jstoi_s")) Module["jstoi_s"] = (() => abort("'jstoi_s' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "getExecutableName")) Module["getExecutableName"] = (() => abort("'getExecutableName' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "listenOnce")) Module["listenOnce"] = (() => abort("'listenOnce' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "autoResumeAudioContext")) Module["autoResumeAudioContext"] = (() => abort("'autoResumeAudioContext' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "dynCallLegacy")) Module["dynCallLegacy"] = (() => abort("'dynCallLegacy' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "getDynCaller")) Module["getDynCaller"] = (() => abort("'getDynCaller' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "dynCall")) Module["dynCall"] = (() => abort("'dynCall' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "callRuntimeCallbacks")) Module["callRuntimeCallbacks"] = (() => abort("'callRuntimeCallbacks' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "wasmTableMirror")) Module["wasmTableMirror"] = (() => abort("'wasmTableMirror' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "setWasmTableEntry")) Module["setWasmTableEntry"] = (() => abort("'setWasmTableEntry' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "getWasmTableEntry")) Module["getWasmTableEntry"] = (() => abort("'getWasmTableEntry' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "handleException")) Module["handleException"] = (() => abort("'handleException' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "runtimeKeepalivePush")) Module["runtimeKeepalivePush"] = (() => abort("'runtimeKeepalivePush' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "runtimeKeepalivePop")) Module["runtimeKeepalivePop"] = (() => abort("'runtimeKeepalivePop' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "callUserCallback")) Module["callUserCallback"] = (() => abort("'callUserCallback' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "maybeExit")) Module["maybeExit"] = (() => abort("'maybeExit' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "safeSetTimeout")) Module["safeSetTimeout"] = (() => abort("'safeSetTimeout' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "asmjsMangle")) Module["asmjsMangle"] = (() => abort("'asmjsMangle' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "asyncLoad")) Module["asyncLoad"] = (() => abort("'asyncLoad' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "alignMemory")) Module["alignMemory"] = (() => abort("'alignMemory' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "mmapAlloc")) Module["mmapAlloc"] = (() => abort("'mmapAlloc' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "reallyNegative")) Module["reallyNegative"] = (() => abort("'reallyNegative' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "unSign")) Module["unSign"] = (() => abort("'unSign' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "reSign")) Module["reSign"] = (() => abort("'reSign' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "formatString")) Module["formatString"] = (() => abort("'formatString' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "PATH")) Module["PATH"] = (() => abort("'PATH' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "PATH_FS")) Module["PATH_FS"] = (() => abort("'PATH_FS' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "SYSCALLS")) Module["SYSCALLS"] = (() => abort("'SYSCALLS' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "syscallMmap2")) Module["syscallMmap2"] = (() => abort("'syscallMmap2' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "syscallMunmap")) Module["syscallMunmap"] = (() => abort("'syscallMunmap' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "getSocketFromFD")) Module["getSocketFromFD"] = (() => abort("'getSocketFromFD' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "getSocketAddress")) Module["getSocketAddress"] = (() => abort("'getSocketAddress' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "JSEvents")) Module["JSEvents"] = (() => abort("'JSEvents' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "registerKeyEventCallback")) Module["registerKeyEventCallback"] = (() => abort("'registerKeyEventCallback' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "specialHTMLTargets")) Module["specialHTMLTargets"] = (() => abort("'specialHTMLTargets' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "maybeCStringToJsString")) Module["maybeCStringToJsString"] = (() => abort("'maybeCStringToJsString' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "findEventTarget")) Module["findEventTarget"] = (() => abort("'findEventTarget' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "findCanvasEventTarget")) Module["findCanvasEventTarget"] = (() => abort("'findCanvasEventTarget' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "getBoundingClientRect")) Module["getBoundingClientRect"] = (() => abort("'getBoundingClientRect' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "fillMouseEventData")) Module["fillMouseEventData"] = (() => abort("'fillMouseEventData' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "registerMouseEventCallback")) Module["registerMouseEventCallback"] = (() => abort("'registerMouseEventCallback' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "registerWheelEventCallback")) Module["registerWheelEventCallback"] = (() => abort("'registerWheelEventCallback' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "registerUiEventCallback")) Module["registerUiEventCallback"] = (() => abort("'registerUiEventCallback' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "registerFocusEventCallback")) Module["registerFocusEventCallback"] = (() => abort("'registerFocusEventCallback' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "fillDeviceOrientationEventData")) Module["fillDeviceOrientationEventData"] = (() => abort("'fillDeviceOrientationEventData' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "registerDeviceOrientationEventCallback")) Module["registerDeviceOrientationEventCallback"] = (() => abort("'registerDeviceOrientationEventCallback' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "fillDeviceMotionEventData")) Module["fillDeviceMotionEventData"] = (() => abort("'fillDeviceMotionEventData' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "registerDeviceMotionEventCallback")) Module["registerDeviceMotionEventCallback"] = (() => abort("'registerDeviceMotionEventCallback' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "screenOrientation")) Module["screenOrientation"] = (() => abort("'screenOrientation' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "fillOrientationChangeEventData")) Module["fillOrientationChangeEventData"] = (() => abort("'fillOrientationChangeEventData' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "registerOrientationChangeEventCallback")) Module["registerOrientationChangeEventCallback"] = (() => abort("'registerOrientationChangeEventCallback' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "fillFullscreenChangeEventData")) Module["fillFullscreenChangeEventData"] = (() => abort("'fillFullscreenChangeEventData' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "registerFullscreenChangeEventCallback")) Module["registerFullscreenChangeEventCallback"] = (() => abort("'registerFullscreenChangeEventCallback' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "registerRestoreOldStyle")) Module["registerRestoreOldStyle"] = (() => abort("'registerRestoreOldStyle' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "hideEverythingExceptGivenElement")) Module["hideEverythingExceptGivenElement"] = (() => abort("'hideEverythingExceptGivenElement' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "restoreHiddenElements")) Module["restoreHiddenElements"] = (() => abort("'restoreHiddenElements' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "setLetterbox")) Module["setLetterbox"] = (() => abort("'setLetterbox' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "currentFullscreenStrategy")) Module["currentFullscreenStrategy"] = (() => abort("'currentFullscreenStrategy' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "restoreOldWindowedStyle")) Module["restoreOldWindowedStyle"] = (() => abort("'restoreOldWindowedStyle' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "softFullscreenResizeWebGLRenderTarget")) Module["softFullscreenResizeWebGLRenderTarget"] = (() => abort("'softFullscreenResizeWebGLRenderTarget' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "doRequestFullscreen")) Module["doRequestFullscreen"] = (() => abort("'doRequestFullscreen' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "fillPointerlockChangeEventData")) Module["fillPointerlockChangeEventData"] = (() => abort("'fillPointerlockChangeEventData' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "registerPointerlockChangeEventCallback")) Module["registerPointerlockChangeEventCallback"] = (() => abort("'registerPointerlockChangeEventCallback' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "registerPointerlockErrorEventCallback")) Module["registerPointerlockErrorEventCallback"] = (() => abort("'registerPointerlockErrorEventCallback' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "requestPointerLock")) Module["requestPointerLock"] = (() => abort("'requestPointerLock' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "fillVisibilityChangeEventData")) Module["fillVisibilityChangeEventData"] = (() => abort("'fillVisibilityChangeEventData' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "registerVisibilityChangeEventCallback")) Module["registerVisibilityChangeEventCallback"] = (() => abort("'registerVisibilityChangeEventCallback' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "registerTouchEventCallback")) Module["registerTouchEventCallback"] = (() => abort("'registerTouchEventCallback' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "fillGamepadEventData")) Module["fillGamepadEventData"] = (() => abort("'fillGamepadEventData' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "registerGamepadEventCallback")) Module["registerGamepadEventCallback"] = (() => abort("'registerGamepadEventCallback' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "registerBeforeUnloadEventCallback")) Module["registerBeforeUnloadEventCallback"] = (() => abort("'registerBeforeUnloadEventCallback' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "fillBatteryEventData")) Module["fillBatteryEventData"] = (() => abort("'fillBatteryEventData' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "battery")) Module["battery"] = (() => abort("'battery' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "registerBatteryEventCallback")) Module["registerBatteryEventCallback"] = (() => abort("'registerBatteryEventCallback' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "setCanvasElementSize")) Module["setCanvasElementSize"] = (() => abort("'setCanvasElementSize' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "getCanvasElementSize")) Module["getCanvasElementSize"] = (() => abort("'getCanvasElementSize' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "demangle")) Module["demangle"] = (() => abort("'demangle' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "demangleAll")) Module["demangleAll"] = (() => abort("'demangleAll' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "jsStackTrace")) Module["jsStackTrace"] = (() => abort("'jsStackTrace' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "stackTrace")) Module["stackTrace"] = (() => abort("'stackTrace' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "getEnvStrings")) Module["getEnvStrings"] = (() => abort("'getEnvStrings' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "checkWasiClock")) Module["checkWasiClock"] = (() => abort("'checkWasiClock' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "writeI53ToI64")) Module["writeI53ToI64"] = (() => abort("'writeI53ToI64' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "writeI53ToI64Clamped")) Module["writeI53ToI64Clamped"] = (() => abort("'writeI53ToI64Clamped' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "writeI53ToI64Signaling")) Module["writeI53ToI64Signaling"] = (() => abort("'writeI53ToI64Signaling' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "writeI53ToU64Clamped")) Module["writeI53ToU64Clamped"] = (() => abort("'writeI53ToU64Clamped' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "writeI53ToU64Signaling")) Module["writeI53ToU64Signaling"] = (() => abort("'writeI53ToU64Signaling' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "readI53FromI64")) Module["readI53FromI64"] = (() => abort("'readI53FromI64' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "readI53FromU64")) Module["readI53FromU64"] = (() => abort("'readI53FromU64' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "convertI32PairToI53")) Module["convertI32PairToI53"] = (() => abort("'convertI32PairToI53' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "convertU32PairToI53")) Module["convertU32PairToI53"] = (() => abort("'convertU32PairToI53' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "setImmediateWrapped")) Module["setImmediateWrapped"] = (() => abort("'setImmediateWrapped' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "clearImmediateWrapped")) Module["clearImmediateWrapped"] = (() => abort("'clearImmediateWrapped' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "polyfillSetImmediate")) Module["polyfillSetImmediate"] = (() => abort("'polyfillSetImmediate' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "uncaughtExceptionCount")) Module["uncaughtExceptionCount"] = (() => abort("'uncaughtExceptionCount' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "exceptionLast")) Module["exceptionLast"] = (() => abort("'exceptionLast' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "exceptionCaught")) Module["exceptionCaught"] = (() => abort("'exceptionCaught' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "ExceptionInfo")) Module["ExceptionInfo"] = (() => abort("'ExceptionInfo' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "CatchInfo")) Module["CatchInfo"] = (() => abort("'CatchInfo' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "exception_addRef")) Module["exception_addRef"] = (() => abort("'exception_addRef' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "exception_decRef")) Module["exception_decRef"] = (() => abort("'exception_decRef' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "Browser")) Module["Browser"] = (() => abort("'Browser' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "funcWrappers")) Module["funcWrappers"] = (() => abort("'funcWrappers' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "getFuncWrapper")) Module["getFuncWrapper"] = (() => abort("'getFuncWrapper' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "setMainLoop")) Module["setMainLoop"] = (() => abort("'setMainLoop' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "wget")) Module["wget"] = (() => abort("'wget' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            Module["FS"] = FS;
            if (!Object.getOwnPropertyDescriptor(Module, "MEMFS")) Module["MEMFS"] = (() => abort("'MEMFS' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "TTY")) Module["TTY"] = (() => abort("'TTY' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "PIPEFS")) Module["PIPEFS"] = (() => abort("'PIPEFS' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "SOCKFS")) Module["SOCKFS"] = (() => abort("'SOCKFS' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "_setNetworkCallback")) Module["_setNetworkCallback"] = (() => abort("'_setNetworkCallback' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "tempFixedLengthArray")) Module["tempFixedLengthArray"] = (() => abort("'tempFixedLengthArray' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "miniTempWebGLFloatBuffers")) Module["miniTempWebGLFloatBuffers"] = (() => abort("'miniTempWebGLFloatBuffers' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "heapObjectForWebGLType")) Module["heapObjectForWebGLType"] = (() => abort("'heapObjectForWebGLType' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "heapAccessShiftForWebGLHeap")) Module["heapAccessShiftForWebGLHeap"] = (() => abort("'heapAccessShiftForWebGLHeap' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "GL")) Module["GL"] = (() => abort("'GL' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "emscriptenWebGLGet")) Module["emscriptenWebGLGet"] = (() => abort("'emscriptenWebGLGet' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "computeUnpackAlignedImageSize")) Module["computeUnpackAlignedImageSize"] = (() => abort("'computeUnpackAlignedImageSize' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "emscriptenWebGLGetTexPixelData")) Module["emscriptenWebGLGetTexPixelData"] = (() => abort("'emscriptenWebGLGetTexPixelData' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "emscriptenWebGLGetUniform")) Module["emscriptenWebGLGetUniform"] = (() => abort("'emscriptenWebGLGetUniform' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "webglGetUniformLocation")) Module["webglGetUniformLocation"] = (() => abort("'webglGetUniformLocation' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "webglPrepareUniformLocationsBeforeFirstUse")) Module["webglPrepareUniformLocationsBeforeFirstUse"] = (() => abort("'webglPrepareUniformLocationsBeforeFirstUse' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "webglGetLeftBracePos")) Module["webglGetLeftBracePos"] = (() => abort("'webglGetLeftBracePos' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "emscriptenWebGLGetVertexAttrib")) Module["emscriptenWebGLGetVertexAttrib"] = (() => abort("'emscriptenWebGLGetVertexAttrib' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "writeGLArray")) Module["writeGLArray"] = (() => abort("'writeGLArray' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "AL")) Module["AL"] = (() => abort("'AL' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "SDL_unicode")) Module["SDL_unicode"] = (() => abort("'SDL_unicode' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "SDL_ttfContext")) Module["SDL_ttfContext"] = (() => abort("'SDL_ttfContext' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "SDL_audio")) Module["SDL_audio"] = (() => abort("'SDL_audio' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "SDL")) Module["SDL"] = (() => abort("'SDL' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "SDL_gfx")) Module["SDL_gfx"] = (() => abort("'SDL_gfx' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "GLUT")) Module["GLUT"] = (() => abort("'GLUT' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "EGL")) Module["EGL"] = (() => abort("'EGL' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "GLFW_Window")) Module["GLFW_Window"] = (() => abort("'GLFW_Window' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "GLFW")) Module["GLFW"] = (() => abort("'GLFW' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "GLEW")) Module["GLEW"] = (() => abort("'GLEW' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "IDBStore")) Module["IDBStore"] = (() => abort("'IDBStore' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "runAndAbortIfError")) Module["runAndAbortIfError"] = (() => abort("'runAndAbortIfError' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "warnOnce")) Module["warnOnce"] = (() => abort("'warnOnce' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "stackSave")) Module["stackSave"] = (() => abort("'stackSave' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "stackRestore")) Module["stackRestore"] = (() => abort("'stackRestore' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "stackAlloc")) Module["stackAlloc"] = (() => abort("'stackAlloc' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "AsciiToString")) Module["AsciiToString"] = (() => abort("'AsciiToString' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "stringToAscii")) Module["stringToAscii"] = (() => abort("'stringToAscii' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "UTF16ToString")) Module["UTF16ToString"] = (() => abort("'UTF16ToString' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "stringToUTF16")) Module["stringToUTF16"] = (() => abort("'stringToUTF16' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "lengthBytesUTF16")) Module["lengthBytesUTF16"] = (() => abort("'lengthBytesUTF16' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "UTF32ToString")) Module["UTF32ToString"] = (() => abort("'UTF32ToString' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "stringToUTF32")) Module["stringToUTF32"] = (() => abort("'stringToUTF32' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "lengthBytesUTF32")) Module["lengthBytesUTF32"] = (() => abort("'lengthBytesUTF32' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "allocateUTF8")) Module["allocateUTF8"] = (() => abort("'allocateUTF8' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            if (!Object.getOwnPropertyDescriptor(Module, "allocateUTF8OnStack")) Module["allocateUTF8OnStack"] = (() => abort("'allocateUTF8OnStack' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)"));
            Module["writeStackCookie"] = writeStackCookie;
            Module["checkStackCookie"] = checkStackCookie;
            Module["ALLOC_NORMAL"] = ALLOC_NORMAL;
            if (!Object.getOwnPropertyDescriptor(Module, "ALLOC_STACK")) Object.defineProperty(Module, "ALLOC_STACK", {
                configurable: true,
                get: function() {
                    abort("'ALLOC_STACK' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the FAQ)")
                }
            });
            var calledRun;

            function ExitStatus(status) {
                this.name = "ExitStatus";
                this.message = "Program terminated with exit(" + status + ")";
                this.status = status
            }
            dependenciesFulfilled = function runCaller() {
                if (!calledRun) run();
                if (!calledRun) dependenciesFulfilled = runCaller
            };

            function stackCheckInit() {
                _emscripten_stack_init();
                writeStackCookie()
            }

            function run(args) {
                args = args || arguments_;
                if (runDependencies > 0) {
                    return
                }
                stackCheckInit();
                preRun();
                if (runDependencies > 0) {
                    return
                }

                function doRun() {
                    if (calledRun) return;
                    calledRun = true;
                    Module["calledRun"] = true;
                    if (ABORT) return;
                    initRuntime();
                    readyPromiseResolve(Module);
                    if (Module["onRuntimeInitialized"]) Module["onRuntimeInitialized"]();
                    assert(!Module["_main"], 'compiled without a main, but one is present. if you added it from JS, use Module["onRuntimeInitialized"]');
                    postRun()
                }
                if (Module["setStatus"]) {
                    Module["setStatus"]("Running...");
                    setTimeout(function() {
                        setTimeout(function() {
                            Module["setStatus"]("")
                        }, 1);
                        doRun()
                    }, 1)
                } else {
                    doRun()
                }
                checkStackCookie()
            }
            Module["run"] = run;

            function exit(status, implicit) {
                EXITSTATUS = status;
                if (keepRuntimeAlive()) {
                    if (!implicit) {
                        var msg = "program exited (with status: " + status + "), but keepRuntimeAlive() is set (counter=" + runtimeKeepaliveCounter + ") due to an async operation, so halting execution but not exiting the runtime or preventing further async execution (you can use emscripten_force_exit, if you want to force a true shutdown)";
                        readyPromiseReject(msg);
                        err(msg)
                    }
                } else {
                    exitRuntime()
                }
                procExit(status)
            }

            function procExit(code) {
                EXITSTATUS = code;
                if (!keepRuntimeAlive()) {
                    if (Module["onExit"]) Module["onExit"](code);
                    ABORT = true
                }
                quit_(code, new ExitStatus(code))
            }
            if (Module["preInit"]) {
                if (typeof Module["preInit"] == "function") Module["preInit"] = [Module["preInit"]];
                while (Module["preInit"].length > 0) {
                    Module["preInit"].pop()()
                }
            }
            run();

            function Prolog(module, args) {
                this.module = module;
                this.args = args;
                this.bindings = {};
                this._bind();
                this._initialise()
            }
            Prolog.prototype._bind = function() {
                this.bindings.PL_atom_chars = this.module.cwrap("PL_atom_chars", "number", ["number"]);
                this.bindings.PL_functor_arity = this.module.cwrap("PL_functor_arity", "number", ["number"]);
                this.bindings.PL_functor_name = this.module.cwrap("PL_functor_name", "number", ["number"]);
                this.bindings.PL_get_functor = this.module.cwrap("PL_get_functor", "number", ["number", "number"]);
                this.bindings.PL_get_chars = this.module.cwrap("PL_get_chars", "number", ["number", "number", "number"]);
                this.bindings.PL_get_arg = this.module.cwrap("PL_get_arg", "number", ["number", "number", "number"]);
                this.bindings.PL_get_integer = this.module.cwrap("PL_get_integer", "number", ["number", "number"]);
                this.bindings.PL_put_chars = this.module.cwrap("PL_put_chars", "number", ["number", "number", "number", "number"]);
                this.bindings.PL_unify = this.module.cwrap("PL_unify", "number", ["number", "number"]);
                this.bindings.PL_is_string = this.module.cwrap("PL_is_string", "number", ["number"]);
                this.bindings.PL_initialise = this.module.cwrap("PL_initialise", "number", ["number", "number"]);
                this.bindings.PL_new_atom = this.module.cwrap("PL_new_atom", "number", ["string"]);
                this.bindings.PL_new_functor = this.module.cwrap("PL_new_functor", "number", ["number", "number"]);
                this.bindings.PL_new_term_ref = this.module.cwrap("PL_new_term_ref", "number", []);
                this.bindings.PL_put_functor = this.module.cwrap("PL_put_functor", "number", ["number", "number"]);
                this.bindings.PL_chars_to_term = this.module.cwrap("PL_chars_to_term", "number", ["string", "number"]);
                this.bindings.PL_call = this.module.cwrap("PL_call", "number", ["number", "number"]);
                this.bindings.PL_unify_arg = this.module.cwrap("PL_unify_arg", "number", ["number", "number", "number"])
            };
            Prolog.prototype._initialise = function() {
                var argv = this.args.map(function(arg) {
                    return this.module.allocate(this.module.intArrayFromString(arg), "i8", this.module.ALLOC_NORMAL)
                }, this);
                var ptr = this.module._malloc(argv.length * 4);
                argv.forEach(function(arg, i) {
                    this.module.setValue(ptr + i * 4, arg, "*")
                }, this);
                if (!this.bindings.PL_initialise(4, ptr)) {
                    throw new Error("SWI-Prolog initialisation failed.")
                }
                this.call_string("assert(user:file_search_path(library, 'wasm-preload/library')).")
            };
            Prolog.prototype.call_string = function(query) {
                var ref = this.new_term_ref();
                if (!this.chars_to_term(query, ref)) {
                    throw new Error("Query has a syntax error: " + query)
                }
                return !!this.call(ref, 0)
            };
            Prolog.prototype.functor_arity = function(functor) {
                return this.bindings.PL_functor_arity(functor)
            };
            Prolog.prototype.functor_name = function(functor) {
                return this.bindings.PL_functor_name(functor)
            };
            Prolog.prototype.get_functor = function(term) {
                var ptr = this.module._malloc(4);
                if (this.bindings.PL_get_functor(term, ptr)) {
                    var functor = this.module.getValue(ptr, "i32");
                    this.module._free(ptr);
                    return functor
                } else {
                    this.module._free(ptr);
                    return null
                }
            };
            Prolog.prototype.get_integer = function(term) {
                var ptr = this.module._malloc(4);
                if (this.bindings.PL_get_integer(term, ptr)) {
                    var number = this.module.getValue(ptr, "i32");
                    this.module._free(ptr);
                    return number
                } else {
                    this.module._free(ptr);
                    return null
                }
            };
            Prolog.prototype.put_chars_string = function(term, string) {
                var len = this.module.lengthBytesUTF8(string) + 1;
                var ptr = this.module._malloc(len);
                this.module.stringToUTF8(string, ptr, len);
                var ret = !!this.bindings.PL_put_chars(term, 5 | 4096, len - 1, ptr);
                this.module._free(ptr);
                return ret
            };
            Prolog.prototype.unify = function(term1, term2) {
                return !!this.bindings.PL_unify(term1, term2)
            };
            Prolog.prototype.is_string = function(term) {
                return !!this.bindings.PL_is_string(term)
            };
            Prolog.prototype.atom_chars = function(atom) {
                var ptr = this.bindings.PL_atom_chars(atom);
                if (ptr === 0) {
                    return null
                } else {
                    return this.module.Pointer_stringify(ptr)
                }
            };
            Prolog.prototype.call = function(term, module) {
                return this.bindings.PL_call(term, module)
            };
            Prolog.prototype.chars_to_term = function(query, t) {
                return this.bindings.PL_chars_to_term(query, t)
            };
            Prolog.prototype.get_chars = function(term) {
                var ptr = this.module._malloc(4);
                var flags = 1 | 2 | 4 | 8 | 16 | 32 | 128 | 4096 | 512;
                if (this.bindings.PL_get_chars(term, ptr, flags)) {
                    return this.module.UTF8ToString(this.module.getValue(ptr, "i32"))
                } else {
                    return null
                }
            };
            Prolog.prototype.get_arg = function(index, term, arg) {
                return this.bindings.PL_get_arg(index, term, arg)
            };
            Prolog.prototype.new_atom = function(string) {
                return this.bindings.PL_new_atom(string)
            };
            Prolog.prototype.new_functor = function(atom, arity) {
                return this.bindings.PL_new_functor(atom, arity)
            };
            Prolog.prototype.new_term_ref = function() {
                return this.bindings.PL_new_term_ref()
            };
            Prolog.prototype.put_functor = function(term, functor) {
                return this.bindings.PL_put_functor(term, functor)
            };
            Prolog.prototype.unify_arg = function(index, term, arg) {
                return this.bindings.PL_unify_arg(index, term, arg)
            };
            Module.onRuntimeInitialized = function() {
                Module.prolog = new Prolog(Module, Module.arguments)
            };


            return SWIPL.ready
        }
    );
})();
if (typeof exports === 'object' && typeof module === 'object')
    module.exports = SWIPL;
else if (typeof define === 'function' && define['amd'])
    define([], function() {
        return SWIPL;
    });
else if (typeof exports === 'object')
    exports["SWIPL"] = SWIPL;
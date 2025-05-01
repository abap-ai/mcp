"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const https_1 = __importDefault(require("https"));
// Disable SSL verification for development
process.env.NODE_TLS_REJECT_UNAUTHORIZED = '0';
https_1.default.globalAgent.options.rejectUnauthorized = false;
//# sourceMappingURL=setup.js.map
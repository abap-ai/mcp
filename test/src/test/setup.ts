import https from 'https';

// Disable SSL verification for development
process.env.NODE_TLS_REJECT_UNAUTHORIZED = '0';
https.globalAgent.options.rejectUnauthorized = false;
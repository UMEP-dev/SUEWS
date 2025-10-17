#!/usr/bin/env node
/**
 * SUEWS MCP Bootstrap Script
 *
 * This script handles first-time setup by:
 * 1. Detecting the platform and selecting appropriate UV binary
 * 2. Creating a Python virtual environment
 * 3. Installing supy using UV
 * 4. Launching the MCP server
 */

const { spawn, exec } = require('child_process');
const fs = require('fs');
const path = require('path');
const os = require('os');

const VENV_PATH = path.join(__dirname, '.venv');

function getUVPath() {
  const platform = os.platform();
  const arch = os.arch();

  const uvMap = {
    'darwin': {
      'arm64': 'uv-darwin-arm64',
      'x64': 'uv-darwin-x86_64'
    },
    'win32': {
      'x64': 'uv-windows-x86_64.exe'
    }
  };

  const uvBinary = uvMap[platform]?.[arch];

  if (!uvBinary) {
    console.error(`Unsupported platform: ${platform}-${arch}`);
    console.error('Please install supy manually: pip install supy');
    process.exit(1);
  }

  return path.join(__dirname, 'bin', uvBinary);
}

function setupEnvironment() {
  console.error('SUEWS MCP: First-time setup (30-60 seconds)...');

  const uv = getUVPath();

  // Make UV executable (Unix-like systems)
  if (os.platform() !== 'win32') {
    try {
      fs.chmodSync(uv, 0o755);
    } catch (error) {
      console.error('Failed to make UV executable:', error.message);
      process.exit(1);
    }
  }

  const venvPython = path.join(
    VENV_PATH,
    os.platform() === 'win32' ? 'Scripts/python.exe' : 'bin/python'
  );

  const setupCmd = `"${uv}" venv "${VENV_PATH}" && "${uv}" pip install --python "${venvPython}" supy "mcp>=0.9.0"`;

  exec(setupCmd, { shell: true }, (error, stdout, stderr) => {
    if (error) {
      console.error('Setup failed:', stderr);
      console.error('Please report this issue: https://github.com/UMEP-dev/SUEWS/issues');
      process.exit(1);
    }

    console.error('Setup complete! Starting SUEWS MCP server...');
    startServer();
  });
}

function startServer() {
  const pythonBin = os.platform() === 'win32' ? 'Scripts/python.exe' : 'bin/python';
  const python = path.join(VENV_PATH, pythonBin);

  const server = spawn(python, ['-m', 'suews_mcp.server'], {
    stdio: 'inherit',
    env: {
      ...process.env,
      PYTHONPATH: path.join(__dirname, 'src')
    }
  });

  server.on('error', (error) => {
    console.error('Failed to start server:', error);
    process.exit(1);
  });

  server.on('exit', (code) => {
    process.exit(code || 0);
  });
}

// Main entry point
if (!fs.existsSync(VENV_PATH)) {
  setupEnvironment();
} else {
  startServer();
}

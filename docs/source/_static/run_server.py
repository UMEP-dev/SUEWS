#!/usr/bin/env python3
"""
Simple HTTP server for testing the SUEWS Configuration UI locally.

This server allows the configuration UI to load the schema file without CORS issues.
"""

import http.server
import socketserver
import os
import sys
from pathlib import Path

def main():
    # Change to the _static directory
    static_dir = Path(__file__).parent
    os.chdir(static_dir)

    PORT = 8080

    # Try to find an available port
    for port in range(PORT, PORT + 10):
        try:
            with socketserver.TCPServer(("", port), http.server.SimpleHTTPRequestHandler) as httpd:
                print(f"🚀 SUEWS Configuration Builder")
                print(f"📁 Serving from: {static_dir}")
                print(f"🌐 Server running at: http://localhost:{port}")
                print(f"🔗 Unified Interface: http://localhost:{port}/index.html")
                print("\n✨ Features:")
                print("   • Modern unified welcome screen and configuration builder")
                print("   • Schema-driven form generation with real-time validation")
                print("   • Export to JSON/YAML formats")
                print("   • Responsive design for desktop and mobile")
                print("\nPress Ctrl+C to stop the server")

                httpd.serve_forever()
        except OSError:
            continue

    print(f"❌ Could not find an available port in range {PORT}-{PORT+9}")
    sys.exit(1)

if __name__ == "__main__":
    main()
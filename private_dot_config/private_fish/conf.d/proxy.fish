# Set up proxy
function proxy_on
  if not set -q _PROXY
      echo "❌ Error: _PROXY variable is undefined. Please check the ~/.env file."
      return 1
  end

  # A custom proxy address can be specified; otherwise, the default value from the .env file will be used.
  set -l proxy $argv[1]
  test -z "$proxy"; and set proxy $_PROXY

  # Set proxy environment variables
  set -g ALL_PROXY $proxy
  set -g HTTP_PROXY $proxy
  set -g HTTPS_PROXY $proxy
  set -g http_proxy $proxy
  set -g https_proxy $proxy
  set -g all_proxy $proxy

  # Set no_proxy environment variables
  if set -q _NO_PROXY
      set -g NO_PROXY $_NO_PROXY
      set -g no_proxy $_NO_PROXY
  end

  echo "✅ Proxy is enabled: $proxy"
end

# Close proxy
function proxy_off
    set -e ALL_PROXY
    set -e HTTP_PROXY
    set -e HTTPS_PROXY
    set -e http_proxy
    set -e https_proxy
    set -e all_proxy
    set -e NO_PROXY
    set -e no_proxy

    echo "❌ Proxy is closed"
end

function proxy_status
    if test -z "$HTTP_PROXY"
        echo "Proxy status: closed"
    else
        echo "Proxy status: enabled"
        echo "HTTP_PROXY: $HTTP_PROXY"
        echo "HTTPS_PROXY: $HTTPS_PROXY"
        echo "ALL_PROXY: $ALL_PROXY"
    end
end

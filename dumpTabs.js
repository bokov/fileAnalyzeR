/*
This javascript dumps the currently opened tabs from a running Firefox instance
that was launched with firefox --start-debugger-server 6000 after setting
the following in about:config...

user_pref("devtools.chrome.enabled", true);
user_pref("devtools.debugger.remote-enabled", true);
user_pref("devtools.debugger.prompt-connection", false);

To open the appropriate console, go to about:debugging, connect localhost:6000
and browse its extensions. Find an extension that already has the right
permissions, such as FoxyTab and then copy-paste this script.

I realize how hacky this it. This is a work in progress as I learn my way around
Firefox internals. I don't recommend you run this on an account or instance you
care about. Educational use only.
*/

async function downloadTabs() {
  const tabs = await browser.tabs.query({});
  const dataStr = "data:text/json;charset=utf-8," + encodeURIComponent(JSON.stringify(tabs, null, 2));
  const downloadAnchorNode = document.createElement('a');
  downloadAnchorNode.setAttribute("href",     dataStr);
  downloadAnchorNode.setAttribute("download", "tabs_data.json");
  document.body.appendChild(downloadAnchorNode); // Required for Firefox
  downloadAnchorNode.click();
  downloadAnchorNode.remove();
}

// Call the function to initiate the download
downloadTabs();

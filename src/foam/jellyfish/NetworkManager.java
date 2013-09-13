package foam.jellyfish;

import java.util.List;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.net.wifi.ScanResult;
import android.net.wifi.WifiManager;
import android.util.Log;
import android.widget.Toast;

import android.net.wifi.WifiManager;
import android.content.BroadcastReceiver;
import android.content.IntentFilter;

import android.net.wifi.WifiConfiguration;

public class NetworkManager {

    public enum State {
        SCANNING, CONNECTED // what else?
    }

    WifiManager wifi;
	BroadcastReceiver receiver;
    State state;
    String SSID;

    NetworkManager(String ssid, Context c) {
		wifi = (WifiManager) c.getSystemService(Context.WIFI_SERVICE);
        state = State.SCANNING;
        SSID = ssid;
        wifi.startScan();
		receiver = new WiFiScanReceiver(SSID, this);
		c.registerReceiver(receiver, new IntentFilter(
                               WifiManager.SCAN_RESULTS_AVAILABLE_ACTION));
    }

    void Connect() {
        Log.i("starwisp", "Attemping connect to "+SSID);

        List<WifiConfiguration> list = wifi.getConfiguredNetworks();

        Boolean found = false;

        for( WifiConfiguration i : list ) {
            if(i.SSID != null && i.SSID.equals("\"" + SSID + "\"")) {
                found = true;
                Log.i("starwisp", "Connecting");
                state=State.CONNECTED;
                wifi.disconnect();
                wifi.enableNetwork(i.networkId, true);
                wifi.reconnect();
                break;
            }
        }

        if (!found) {
            Log.i("starwisp", "adding wifi config");
            WifiConfiguration conf = new WifiConfiguration();
            conf.SSID = "\"" + SSID + "\"";

//conf.wepKeys[0] = "\"" + networkPass + "\"";
//conf.wepTxKeyIndex = 0;
//conf.allowedKeyManagement.set(WifiConfiguration.KeyMgmt.NONE);
//conf.allowedGroupCiphers.set(WifiConfiguration.GroupCipher.WEP40);

            conf.allowedKeyManagement.set(WifiConfiguration.KeyMgmt.NONE);
            wifi.addNetwork(conf);
        }
    }

    private class WiFiScanReceiver extends BroadcastReceiver {
        private static final String TAG = "WiFiScanReceiver";

        public WiFiScanReceiver(String ssid, NetworkManager netm) {
            super();
            SSID=ssid;
            nm = netm;
        }

        public String SSID;
        public NetworkManager nm;

        @Override
        public void onReceive(Context c, Intent intent) {
            List<ScanResult> results = nm.wifi.getScanResults();
            ScanResult bestSignal = null;

            if (nm.state==State.SCANNING) {
                Log.i("starwisp", "Scanning "+nm.state);


                for (ScanResult result : results) {
                    if (result.SSID.equals(SSID)) {
                        nm.Connect();
                        return;
                    }
                }

                /*   if (nm.state==State.SCANNING) {
                    Log.i("starwisp", "REScanning "+nm.state);

                    nm.wifi.startScan();
                    }*/
            }
        }
    }
}

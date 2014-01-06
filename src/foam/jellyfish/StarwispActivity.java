// Starwisp Copyright (C) 2013 Dave Griffiths
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

package foam.jellyfish;

import java.util.ArrayList;

import android.support.v4.app.FragmentActivity;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.content.Context;
import android.view.ViewGroup;
import android.view.View;
import android.graphics.Typeface;

import tv.ouya.console.api.OuyaController;
import android.view.InputDevice;
import android.view.MotionEvent;

import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONArray;

public class StarwispActivity extends FragmentActivity
{
    public String m_Name;
    static public Scheme m_Scheme;
    static public StarwispBuilder m_Builder;
    public Typeface m_Typeface;
    static public String m_AppDir;

    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.main);
        OuyaController.init(this);

        String arg = "none";
        Bundle extras = getIntent().getExtras();
        if (extras!=null) {
            arg = extras.getString("arg");
        }

        String json = m_Scheme.eval("(activity-callback 'on-create \""+m_Name+"\" (list \""+arg+"\"))");
        View root = findViewById(R.id.main);

        m_Typeface = Typeface.createFromAsset(getAssets(), "fonts/Pfennig.ttf");
        //m_Typeface = Typeface.createFromAsset(getAssets(), "fonts/grstylus.ttf");

        try {
            m_Builder.Build(this, m_Name, new JSONArray(json), (ViewGroup) root);
        } catch (JSONException e) {
            Log.e("starwisp", "Error parsing ["+json+"] " + e.toString());
        }
    }

    @Override
    public boolean onGenericMotionEvent(final MotionEvent event) {
        //Get the player #
        int player = OuyaController.getPlayerNumByDeviceId(event.getDeviceId());    

        Log.i("starwisp","ogme");
        
        // Joystick
        if((event.getSource() & InputDevice.SOURCE_CLASS_JOYSTICK) != 0) {
            //Get all the axis for the event
            float LS_X = event.getAxisValue(OuyaController.AXIS_LS_X);
            float LS_Y = event.getAxisValue(OuyaController.AXIS_LS_Y);
            
            float RS_X = event.getAxisValue(OuyaController.AXIS_RS_X);
            float RS_Y = event.getAxisValue(OuyaController.AXIS_RS_Y);
            float L2 = event.getAxisValue(OuyaController.AXIS_L2);
            float R2 = event.getAxisValue(OuyaController.AXIS_R2);
            
            Log.i("starwisp","controller "+LS_X+" "+LS_Y+
                  RS_X+" "+RS_Y+
                  L2+" "+R2);
            
            Scheme.eval("(on-fling "+LS_X*-500+" "+LS_Y*-500+")");   
        }

        //Touchpad
        if((event.getSource() & InputDevice.SOURCE_CLASS_POINTER) != 0) {
            //Print the pixel coordinates of the cursor
            Log.i("starwisp", "Cursor X: " + event.getX() + "Cursor Y: " + event.getY());
        }
        
        return true;
    }



    @Override
    public void onStart()
    {
        super.onStart();

        String arg = "none";
        Bundle extras = getIntent().getExtras();
        if (extras!=null) {
            arg = extras.getString("arg");
        }

        String ret=m_Scheme.eval("(activity-callback 'on-start \""+m_Name+"\" (list \""+arg+"\"))");
        try {
            m_Builder.UpdateList(this, m_Name, new JSONArray(ret));
        } catch (JSONException e) {
            Log.e("starwisp", "Error parsing ["+ret+"] " + e.toString());
        }
    }

    @Override
    public void onResume()
    {
        super.onResume();
        String ret=m_Scheme.eval("(activity-callback 'on-resume \""+m_Name+"\" '())");
        try {
            m_Builder.UpdateList(this, m_Name, new JSONArray(ret));
        } catch (JSONException e) {
            Log.e("starwisp", "Error parsing ["+ret+"] " + e.toString());
        }
    }


    @Override
    public void onPause()
    {
        super.onPause();
        String ret=m_Scheme.eval("(activity-callback 'on-pause \""+m_Name+"\" '())");
        try {
            m_Builder.UpdateList(this, m_Name, new JSONArray(ret));
        } catch (JSONException e) {
            Log.e("starwisp", "Error parsing ["+ret+"] " + e.toString());
        }
    }

    @Override
    public void onStop()
    {
        super.onStop();
        String ret=m_Scheme.eval("(activity-callback 'on-stop \""+m_Name+"\" '())");
        try {
            m_Builder.UpdateList(this, m_Name, new JSONArray(ret));
        } catch (JSONException e) {
            Log.e("starwisp", "Error parsing ["+ret+"] " + e.toString());
        }
    }

    @Override
    public void onDestroy()
    {
        super.onDestroy();
        String ret=m_Scheme.eval("(activity-callback 'on-destroy \""+m_Name+"\" '())");
        try {
            m_Builder.UpdateList(this, m_Name, new JSONArray(ret));
        } catch (JSONException e) {
            Log.e("starwisp", "Error parsing ["+ret+"] " + e.toString());
        }
    }

    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        String ret=m_Scheme.eval("(activity-callback 'on-activity-result \""+m_Name+"\" '("+requestCode+" "+resultCode+"))");
        try {
            m_Builder.UpdateList(this, m_Name, new JSONArray(ret));
        } catch (JSONException e) {
            Log.e("starwisp", "Error parsing ["+ret+"] " + e.toString());
        }
	}

    @Override
    protected void onSaveInstanceState(Bundle outState) {
        //No call for super(). Bug on API Level > 11.
    }

}

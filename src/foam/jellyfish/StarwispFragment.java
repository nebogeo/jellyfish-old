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
import android.support.v4.app.Fragment;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.content.Context;
import android.view.ViewGroup;
import android.view.View;
import android.view.LayoutInflater;
import android.graphics.Typeface;


import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONArray;

public class StarwispFragment extends Fragment
{
    public String m_Name;
//    static public Scheme m_Scheme;
//    static public StarwispBuilder m_Builder;
//    public Typeface m_Typeface;
//    static public String m_AppDir;

    StarwispActivity m_Owner;

    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        m_Owner = (StarwispActivity)getActivity();
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup root, Bundle savedInstanceState)
    {
        super.onCreateView(inflater,root,savedInstanceState);

        String arg = "none";
        String json = m_Owner.m_Scheme.eval("(fragment-callback 'on-create \""+m_Name+"\" (list \""+arg+"\"))");
        View view = inflater.inflate(R.layout.main_fragment, root, false);

        try {
            m_Owner.m_Builder.Build(m_Owner, m_Name, new JSONArray(json), (ViewGroup) view);
        } catch (JSONException e) {
            Log.e("starwisp", "Error parsing ["+json+"] " + e.toString());
        }

        return view;
    }


    @Override
    public void onStart()
    {
        super.onStart();

        String arg = "none";
        String ret=m_Owner.m_Scheme.eval("(fragment-callback 'on-start \""+m_Name+"\" (list \""+arg+"\"))");
        try {
            m_Owner.m_Builder.UpdateList(m_Owner, m_Name,  new JSONArray(ret));
        } catch (JSONException e) {
            Log.e("starwisp", "Error parsing ["+ret+"] " + e.toString());
        }
    }

    @Override
    public void onResume()
    {
        super.onResume();
        String ret=m_Owner.m_Scheme.eval("(fragment-callback 'on-resume \""+m_Name+"\" '())");
        try {
            m_Owner.m_Builder.UpdateList(m_Owner, m_Name, new JSONArray(ret));
        } catch (JSONException e) {
            Log.e("starwisp", "Error parsing ["+ret+"] " + e.toString());
        }
    }


    @Override
    public void onPause()
    {
        super.onPause();
        String ret=m_Owner.m_Scheme.eval("(fragment-callback 'on-pause \""+m_Name+"\" '())");
        try {
            m_Owner.m_Builder.UpdateList(m_Owner, m_Name, new JSONArray(ret));
        } catch (JSONException e) {
            Log.e("starwisp", "Error parsing ["+ret+"] " + e.toString());
        }
    }

    @Override
    public void onStop()
    {
        super.onStop();
        String ret=m_Owner.m_Scheme.eval("(fragment-callback 'on-stop \""+m_Name+"\" '())");
        try {
            m_Owner.m_Builder.UpdateList(m_Owner, m_Name, new JSONArray(ret));
        } catch (JSONException e) {
            Log.e("starwisp", "Error parsing ["+ret+"] " + e.toString());
        }
    }

    @Override
    public void onDestroy()
    {
        super.onDestroy();
        String ret=m_Owner.m_Scheme.eval("(fragment-callback 'on-destroy \""+m_Name+"\" '())");
        try {
            m_Owner.m_Builder.UpdateList(m_Owner, m_Name, new JSONArray(ret));
        } catch (JSONException e) {
            Log.e("starwisp", "Error parsing ["+ret+"] " + e.toString());
        }
    }

    @Override
    public void onSaveInstanceState(Bundle outState) {
        //No call for super(). Bug on API Level > 11.
    }

}

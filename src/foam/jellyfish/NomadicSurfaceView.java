// Copyright (C) 2012 Dave Griffiths
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

package foam.jellyfish;

import javax.microedition.khronos.egl.EGLConfig;
import javax.microedition.khronos.opengles.GL10;

import android.content.Context;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.InputStream;
import java.io.StringWriter;
import java.io.File;
import java.io.Writer;
import java.io.Reader;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.FileInputStream;
import java.io.ByteArrayOutputStream;
import java.nio.ByteBuffer;
import android.widget.EditText;
import android.app.AlertDialog;

import android.util.Log;

import java.io.IOException;
import java.io.FileNotFoundException;

import android.opengl.GLSurfaceView;
import android.view.MotionEvent;
import android.content.DialogInterface;
import android.view.MotionEvent;
import android.view.View;
import android.view.GestureDetector;
import android.view.GestureDetector.SimpleOnGestureListener;

class NomadicSurfaceView extends GLSurfaceView {

    public NomadicRenderer mRenderer;
    Context mAct;
    String mFilename;
    GestureDetector mGestures;

    public NomadicSurfaceView(StarwispActivity context, int id) {
        super(context);
        Log.i("starwisp","creating surface view");
        mAct = context;
        mFilename="";        
        mGestures = new GestureDetector(context, new GestureListener());  
        mRenderer = new NomadicRenderer(context, id);
        setRenderer(mRenderer);
    }
    
    public boolean onTouchEvent(MotionEvent event) {
        mGestures.onTouchEvent(event);
        return true;
    }

    private class GestureListener 
        extends GestureDetector.SimpleOnGestureListener 
    {  
        public GestureListener() {}

        @Override  
        public boolean onFling(MotionEvent e1, MotionEvent e2,  
                               final float vx, final float vy) {  
            Scheme.eval("(on-fling "+vx+" "+vy+")");
            return true;  
        }  
    }  


    private static native void nativePause();
}

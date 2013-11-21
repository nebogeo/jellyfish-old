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
import android.opengl.GLSurfaceView;
import android.app.AlertDialog;
import android.graphics.BitmapFactory;
import android.graphics.Bitmap;
import android.util.Log;
import android.os.Environment;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.InputStream;
import java.io.StringWriter;
import java.io.File;
import java.io.Writer;
import java.io.Reader;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.InputStreamReader;
import java.io.FileInputStream;
import java.io.ByteArrayOutputStream;
import java.nio.ByteBuffer;

class NomadicRenderer implements GLSurfaceView.Renderer {
    static class Lock extends Object {}
    static public Lock mLock = new Lock();

    int mID;
    StarwispActivity mCtx;

    public NomadicRenderer(StarwispActivity ctx, int ID)
    {
        mAct=ctx;
        mID=ID;
        mCtx=ctx;
    }

    public void onSurfaceCreated(GL10 gl, EGLConfig config) {
        Log.i("starwisp","GL startup");
        Scheme.initGL();
        Log.i("starwisp","Running callback");
        Scheme.evalPre("(widget-callback \""+mCtx.m_Name+"\" "+mID+" '())");
    }

    public void onSurfaceChanged(GL10 gl, int w, int h) {
        Scheme.resize(w, h);
    }

    public void onDrawFrame(GL10 gl) {
        synchronized (mLock)
        {
//            eval("(input-sensor (list "+mSensorX+" "+mSensorY+" "+mSensorZ+"))");
            Scheme.render();
        }
    }


    public void loadTexture(String texname) {
        FlxImage tex=readRawImage(mAct,texname);
        if (tex!=null)
        {
            synchronized (mLock)
            {
                Scheme.loadTexture(texname,tex.mData,tex.mWidth,tex.mHeight);
            }
        }
    }

    Context mAct;

    public FlxImage readRawImage(Context ctx, String fn)
    {
        InputStream fis = null;
        try
        {
            fis = ctx.getAssets().open(fn);
            Bitmap bmp = BitmapFactory.decodeStream(fis);
            ByteBuffer bb = ByteBuffer.allocate(bmp.getWidth()*bmp.getHeight()*4);
            bmp.copyPixelsToBuffer(bb);
            FlxImage ret=new FlxImage();
            ret.mWidth = bmp.getWidth();
            ret.mHeight = bmp.getHeight();
            ret.mData = bb.array();
            return ret;
        }
        catch(FileNotFoundException e)
        {
            AlertDialog.Builder alert = new AlertDialog.Builder(mAct);
            alert.setTitle(e.toString());
            alert.show();
        }
        catch(IOException ioe)
        {
            AlertDialog.Builder alert = new AlertDialog.Builder(mAct);
            alert.setTitle(ioe.toString());
            alert.show();
        }
        return null;
    }

    public static String readRawTextFileExternal(Context ctx, String fn)
    {
        File f = new File(fn);
        if (f.exists()) {
            try {
                StringBuffer inLine = new StringBuffer();
                FileReader fr = new FileReader(f);
                BufferedReader br = new BufferedReader(fr);
                String text;
                while ((text = br.readLine()) != null) {
                    inLine.append(text);
                    inLine.append("\n");
                }
                return inLine.toString();
            }
            catch(Exception e)
            {
                return "";
            }
        }
        return "";
    }

    public void writeRawTextFileExternal(Context ctx, String fn, String code)
    {
        try {
            File sdCardFile = Environment.getExternalStorageDirectory();
            if (sdCardFile.canWrite() == true) {
//                File viewerFile = new File(sdCardFile, "Viewer");
//                viewerFile.mkdirs();
//                File textFile = new File(viewerFile, fn);
                File textFile = new File(fn);
                FileWriter fileWriter = new FileWriter(textFile);
                BufferedWriter bufferedWriter = new BufferedWriter(fileWriter);
                try {
                    Log.i("fluxus","writing "+code);
                    bufferedWriter.write(code);
                } finally {
                    bufferedWriter.close();
                }
            } else {
                Log.e("fluxus", "Cannot write to SD Card");
            }
        } catch (Exception e) {
            Log.e("fluxus", "Error - " + e);
            e.printStackTrace();
        }
    }


    public static String readRawTextFile(Context ctx, String fn)
    {
        BufferedReader inRd=null;
        try
        {
            StringBuffer inLine = new StringBuffer();
            inRd =
                new BufferedReader(new InputStreamReader
                                   (ctx.getAssets().open(fn)));

            String text;
            while ((text = inRd.readLine()) != null) {
                inLine.append(text);
                inLine.append("\n");
            }

            return inLine.toString();
        }
        catch (IOException e)
        {
            return "";
        }
        finally
        {
            try { inRd.close(); }
            catch (IOException e) { return ""; }
        }
    }

}

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

import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;

import android.app.Activity;
import android.util.Log;
import android.content.Context;
import android.graphics.BitmapFactory;
import android.graphics.Bitmap;
import java.io.FileNotFoundException;
import java.nio.ByteBuffer;
import java.io.InputStream;


public class Scheme
{
    static class Lock extends Object {}
    static public Lock mLock = new Lock();

    private static native void nativeInit();
    private static native void nativeInitGL();
    private static native void nativeDone();
    private static native String nativeEval(String code);
    private static native void nativeResize(int w, int h);
    private static native void nativeRender();
    private static native void nativeLoadTexture(String texname, byte[] arr, int w, int h);

    static Activity m_Act;
    static FlxImage ret;

    static {
        System.loadLibrary("starwisp-core");
    }

    public Scheme(Activity ctx) {
        m_Act = ctx;
        Log.i("starwisp","starting up...");
        nativeInit();
        Log.i("starwisp","started, now running init.scm...");
        eval(readRawTextFile(ctx, "init.scm"));
        Log.i("starwisp","running lib.scm...");
        eval(readRawTextFile(ctx, "lib.scm"));
        Log.i("starwisp","running jellyfish.scm...");
        eval(readRawTextFile(ctx, "jellyfish.scm"));
        Log.i("starwisp","running boot.scm...");
        eval(readRawTextFile(ctx, "boot.scm"));
        Log.i("starwisp","running compiler.scm...");
        eval(readRawTextFile(ctx, "compiler.scm"));
        Log.i("starwisp","done.");

        ret = new FlxImage();
    }

    public static void initGL() {
        synchronized (mLock) { 
            nativeInitGL(); 
            createEngine();

            Log.i("starwisp","loading textures");
            loadTexture("stripes.png");
            Log.i("starwisp","done.");
        } 
    }

    public static void resize(int w, int h) { synchronized (mLock) { nativeResize(w,h); } }
    public static void render() { synchronized (mLock) { nativeRender(); } }

    public class FlxImage
    {
        byte [] mData;
        int mWidth;
        int mHeight;
    }

    public static void loadTexture(String texname) {
        FlxImage tex=readRawImage(m_Act,texname);
        if (tex!=null)
        {
            synchronized (mLock)
            {
                Log.i("starwisp","calling native load texture");
                nativeLoadTexture(texname,tex.mData,tex.mWidth,tex.mHeight);
            }
        }
    }


    public static FlxImage readRawImage(Context ctx, String fn)
    {
        InputStream fis = null;
        try
        {
            fis = ctx.getAssets().open(fn);
            Bitmap bmp = BitmapFactory.decodeStream(fis);
            ByteBuffer bb = ByteBuffer.allocate(bmp.getWidth()*bmp.getHeight()*4);
            bmp.copyPixelsToBuffer(bb);
            ret.mWidth = bmp.getWidth();
            ret.mHeight = bmp.getHeight();
            ret.mData = bb.array();
            Log.i("starwisp","loaded ok");
            return ret;
        }
        catch(FileNotFoundException e)
        {
            Log.i("starwisp","fnf");
        }
        catch(IOException ioe)
        {
            Log.i("starwisp","ioe");
        }
        return null;
    }


    public static native void createEngine();

    public static String eval(String code) {
        synchronized (mLock)
        {
            return nativeEval(code);
        }
    }

    public static String evalPre(String code) {
        synchronized (mLock)
        {
            return nativeEval("(pre-process-run '("+code+"))");
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
            int read=0;
            while ((text = inRd.readLine()) != null) {
                read+=text.length()+1;
                inLine.append(text);
                inLine.append("\n");
            }

            String ret = inLine.toString();

            return ret;

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

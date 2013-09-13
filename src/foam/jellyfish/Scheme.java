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

public class Scheme
{
    static class Lock extends Object {}
    static public Lock mLock = new Lock();

    private static native void nativeInit();
    private static native void nativeDone();
    private static native String nativeEval(String code);

    static {
        System.loadLibrary("starwisp-core");
    }

    public Scheme(Activity ctx) {
        Log.i("starwisp","starting up...");
        nativeInit();
        Log.i("starwisp","started, now running init.scm...");
        eval(readRawTextFile(ctx, "init.scm"));
        eval(readRawTextFile(ctx, "lib.scm"));
    }

    public String eval(String code) {
        synchronized (mLock)
        {
            return nativeEval(code);
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

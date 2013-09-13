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

import java.lang.Thread;
import android.view.SurfaceView;
import android.view.SurfaceHolder;
import android.graphics.Canvas;

class StarwispCanvasThread extends Thread {
    private SurfaceHolder m_SurfaceHolder;
    private StarwispCanvas m_Canvas;
    private boolean m_Run = false;

    public StarwispCanvasThread(SurfaceHolder surfaceHolder, StarwispCanvas canvas) {
        m_SurfaceHolder = surfaceHolder;
        m_Canvas = canvas;
    }

    public void setRunning(boolean run) {
        m_Run = run;
    }

    @Override
    public void run() {
        Canvas c;
        while (m_Run) {
            c = null;
            try {
                c = m_SurfaceHolder.lockCanvas(null);
                synchronized (m_SurfaceHolder) {
                    m_Canvas.postInvalidate();
                }

            } finally {
                if (c != null) {
                    m_SurfaceHolder.unlockCanvasAndPost(c);
                }
            }

            try {
                Thread.sleep(500);
            } catch (InterruptedException e) { }

        }
    }
}

package foam.jellyfish;

import java.io.File;
import java.io.FileOutputStream;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import android.app.Activity;
import android.content.pm.PackageManager;
import android.hardware.Camera;
import android.hardware.Camera.CameraInfo;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.view.Surface;
import android.widget.Toast;
import java.io.IOException;
import android.view.SurfaceHolder;
import android.view.SurfaceView;
import android.view.Display;
import android.view.WindowManager;
import android.content.Context;
import android.hardware.Camera.Parameters;
import android.hardware.Camera.PictureCallback;
import android.hardware.Camera.Size;

public class CameraPreview extends SurfaceView implements
        SurfaceHolder.Callback {

    private SurfaceHolder mSurfaceHolder;
    PictureTaker mPictureTaker;
    Context mCtx;

    // Constructor that obtains context and camera
    @SuppressWarnings("deprecation")
    public CameraPreview(Context context, PictureTaker picturetaker) {
        super(context);
        mCtx=context;
        mPictureTaker=picturetaker;
        this.mSurfaceHolder = this.getHolder();
        this.mSurfaceHolder.addCallback(this);
        this.mSurfaceHolder.setType(SurfaceHolder.SURFACE_TYPE_PUSH_BUFFERS);
    }

    public void TakePicture(PictureCallback cb) {
        mPictureTaker.TakePicture(this,cb);
    }

    public void Shutdown() {
        mPictureTaker.Shutdown();
    }

    @Override
    public void surfaceCreated(SurfaceHolder surfaceHolder) {
        mSurfaceHolder=surfaceHolder;
        mPictureTaker.Startup(this);
    }

    @Override
    public void surfaceDestroyed(SurfaceHolder surfaceHolder) {
    }

    @Override
    public void surfaceChanged(SurfaceHolder surfaceHolder, int format,
            int width, int height) {
        mSurfaceHolder=surfaceHolder;
        mPictureTaker.mCam.stopPreview();

        Parameters parameters = mPictureTaker.mCam.getParameters();
        List<Size> list = parameters.getSupportedPictureSizes();

        int minHeight=9999999;
        int minWidth=0;
        for(int i = 0; i<list.size(); i++){
            if(list.get(i).height<minHeight){
                minHeight = list.get(i).height;
                minWidth = list.get(i).width;
            }
        }

        Log.i("starwisp","camera layout: "+minWidth+" "+minHeight);

        parameters.setPreviewSize(minWidth, minHeight);
        mPictureTaker.mCam.setParameters(parameters);
        mPictureTaker.mCam.startPreview();

/*
        Parameters parameters = mPictureTaker.mCam.getParameters();
        Display display = ((WindowManager)mCtx.getSystemService(Context.WINDOW_SERVICE)).getDefaultDisplay();

        if(display.getRotation() == Surface.ROTATION_0)
        {
            parameters.setPreviewSize(height, width);
            mPictureTaker.mCam.setDisplayOrientation(90);
        }

        if(display.getRotation() == Surface.ROTATION_90)
        {
            parameters.setPreviewSize(width, height);
        }

        if(display.getRotation() == Surface.ROTATION_180)
        {
            parameters.setPreviewSize(height, width);
        }

        if(display.getRotation() == Surface.ROTATION_270)
        {
            parameters.setPreviewSize(width, height);
            mPictureTaker.mCam.setDisplayOrientation(180);
        }

        mPictureTaker.mCam.setParameters(parameters);
        mPictureTaker.mCam.startPreview();
*/

    }
}

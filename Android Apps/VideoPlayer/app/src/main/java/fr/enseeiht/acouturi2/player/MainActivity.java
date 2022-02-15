package fr.enseeiht.acouturi2.player;

import static java.lang.Math.floor;
import static java.lang.Math.round;
import static java.lang.Thread.sleep;

import androidx.annotation.NonNull;
import androidx.annotation.OptIn;
import androidx.appcompat.app.AppCompatActivity;
import androidx.loader.content.AsyncTaskLoader;

import android.content.Intent;
import android.media.MediaPlayer;
import android.net.Uri;
import android.os.AsyncTask;
import android.os.Build;
import android.os.Bundle;
import android.view.SurfaceHolder;
import android.view.SurfaceView;
import android.view.View;
import android.widget.Button;
import android.widget.SeekBar;
import android.widget.TextView;

import java.io.IOException;

public class MainActivity extends AppCompatActivity implements SurfaceHolder.Callback {

    private static final int SELECT_VIDEO = 100;
    private Button selectVideoButton;
    private Button intentPlayButton;
    private Button playFromStartButton;
    private Button pauseButton;
    private SurfaceView surfaceView;
    private TextView url;
    private String uri;
    private MediaPlayer player;
    private SurfaceHolder surfaceHolder;
    protected SeekBar seekBar;
    private BarUpdaterTask barUpdater;

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
        uri = data.getDataString();
        url.setText(uri);
    }

    public void surfaceCreated(SurfaceHolder holder){
        player.setDisplay(surfaceHolder);
    }

    @Override
    public void surfaceChanged(@NonNull SurfaceHolder surfaceHolder, int i, int i1, int i2) {
    }

    @Override
    public void surfaceDestroyed(@NonNull SurfaceHolder surfaceHolder) {
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        selectVideoButton = (Button) findViewById(R.id.selectVideoButton);
        intentPlayButton = (Button) findViewById(R.id.IntentPlayButton);
        selectVideoButton = (Button) findViewById(R.id.selectVideoButton);
        pauseButton = (Button) findViewById(R.id.pauseButton);
        surfaceView = (SurfaceView) findViewById(R.id.surfaceView);
        url = (TextView) findViewById(R.id.textURI);
        playFromStartButton = (Button) findViewById(R.id.playFromStartButton);
        seekBar = (SeekBar) findViewById(R.id.seekBar);

        player = new MediaPlayer();
        barUpdater = new BarUpdaterTask();

        surfaceHolder = surfaceView.getHolder();
        surfaceHolder.addCallback(this);
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.HONEYCOMB){
            surfaceHolder.setType(SurfaceHolder.SURFACE_TYPE_PUSH_BUFFERS);
        }

        selectVideoButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                Intent intent = new Intent(Intent.ACTION_PICK);
                intent.setDataAndType(Uri.parse("storage/self"), "video/*");
                startActivityForResult(intent, SELECT_VIDEO);
            }
        });

        intentPlayButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                startActivity(new Intent(Intent.ACTION_VIEW, Uri.parse(uri)));
            }
        });

        playFromStartButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                if (player.isPlaying()){
                    player.pause();
                }
                player.reset();
                try {
                    player.setDataSource(MainActivity.this, Uri.parse(uri));
                    player.prepare();
                    player.setOnPreparedListener(new MediaPlayer.OnPreparedListener() {
                        @Override
                        public void onPrepared(MediaPlayer mediaPlayer) {
                            player.start();
                            barUpdater.execute();
                        }
                    });
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        });

        pauseButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                if (player.isPlaying()){
                    player.pause();
                } else {
                    player.start();
                }
            }
        });

        seekBar.setOnSeekBarChangeListener(new SeekBar.OnSeekBarChangeListener() {
            @Override
            public void onProgressChanged(SeekBar seekBar, int i, boolean b) {
                //System.out.println(seekBar.getProgress());
                if (b) {
                    player.seekTo(player.getDuration()*seekBar.getProgress()/100);
                }
            }

            @Override
            public void onStartTrackingTouch(SeekBar seekBar) {
            }

            @Override
            public void onStopTrackingTouch(SeekBar seekBar) {
            }
        });
    }

    @Override
    public void onPointerCaptureChanged(boolean hasCapture) {
        super.onPointerCaptureChanged(hasCapture);
    }

    class BarUpdaterTask extends AsyncTask{
        int position;
        int duration;
        float progression;
        @Override
        protected Object doInBackground(Object[] objects) {
            while (!isCancelled()) {
                try {
                    position = player.getCurrentPosition();
                    duration = player.getDuration();
                    publishProgress();
                    sleep(50);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
            return null;
        }

        @Override
        protected void onProgressUpdate(Object[] values) {
            progression = (position*100/duration);
            seekBar.setProgress(round(progression));
        }
    }
}

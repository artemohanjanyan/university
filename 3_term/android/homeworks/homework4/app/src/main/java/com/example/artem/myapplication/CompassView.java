package com.example.artem.myapplication;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.util.AttributeSet;
import android.view.View;

/**
 * View which draws compass based on provided angle
 */
public class CompassView extends View {
    private double angle;
    private double radius;
    private double centerX, centerY;

    private Paint bluePaint = new Paint();
    private Paint redPaint = new Paint();

    public CompassView(Context context, AttributeSet attrs) {
        super(context, attrs);

        angle = 0;

        bluePaint.setColor(Color.BLUE);
        bluePaint.setStrokeWidth(10);
        redPaint.setColor(Color.RED);
        redPaint.setStrokeWidth(10);
    }

    public double getAngle() {
        return angle;
    }

    public void setAngle(double angle) {
        this.angle = angle;
    }

    @Override
    protected void onSizeChanged(int w, int h, int oldw, int oldh) {
        super.onSizeChanged(w, h, oldw, oldh);
        centerX = ((double) w) / 2;
        centerY = ((double) h) / 2;
        radius = 0.8 * Math.min(centerX, centerY);
    }

    @Override
    protected void onDraw(Canvas canvas) {
        super.onDraw(canvas);

        // Red points to north
        canvas.drawLine((float) centerX, (float) centerY,
                (float) (centerX + Math.cos(angle) * radius),
                (float) (centerY - Math.sin(angle) * radius),
                redPaint);
        // Blue points to south
        canvas.drawLine((float) centerX, (float) centerY,
                (float) (centerX - Math.cos(angle) * radius),
                (float) (centerY + Math.sin(angle) * radius),
                bluePaint);

        invalidate();
    }
}

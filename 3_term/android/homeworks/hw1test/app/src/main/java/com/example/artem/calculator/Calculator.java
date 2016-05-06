package com.example.artem.calculator;

import android.os.Bundle;
import android.support.v7.app.AppCompatActivity;
import android.text.Layout;
import android.text.method.ScrollingMovementMethod;
import android.view.View;
import android.widget.Button;
import android.widget.TextView;

import java.math.BigDecimal;

public class Calculator extends AppCompatActivity {

    static class DataHolder {
        private BigDecimal firstNum;
        private Functions.Function function;

        private StringBuilder secondNumStr;
        private boolean isDotEntered;

        private CharSequence textViewStr;

        public DataHolder(BigDecimal firstNum, Functions.Function function,
                          StringBuilder secondNumStr, boolean isDotEntered,
                          CharSequence textViewStr) {
            this.firstNum = firstNum;
            this.function = function;
            this.secondNumStr = secondNumStr;
            this.isDotEntered = isDotEntered;
            this.textViewStr = textViewStr;
        }
    }

    private BigDecimal firstNum;
    private Functions.Function function;

    private StringBuilder secondNumStr;
    private boolean isDotEntered;

    private TextView textView;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.test1);
        //setContentView(R.layout.activity_calculator);

        textView = (TextView) findViewById(R.id.text);
        textView.setMovementMethod(new ScrollingMovementMethod());

        DataHolder dataHolder = (DataHolder) getLastCustomNonConfigurationInstance();
        if (dataHolder != null) {
            firstNum = dataHolder.firstNum;
            function = dataHolder.function;
            secondNumStr = dataHolder.secondNumStr;
            isDotEntered = dataHolder.isDotEntered;
            textView.setText(dataHolder.textViewStr);
        } else {
            onClearClicked(null);
        }
    }

    @Override
    public Object onRetainCustomNonConfigurationInstance() {
        return new DataHolder(
                firstNum, function, secondNumStr, isDotEntered, textView.getText());
    }

    /*
        Helper functions
     */

    private void clearSecondNum() {
        secondNumStr = new StringBuilder();
        isDotEntered = false;
    }

    private void scrollToBottom() {
        Layout layout = textView.getLayout();
        if (layout != null) {
            int bottomScroll = layout.getLineBottom(textView.getLineCount() - 1) - textView.getHeight();
            if (bottomScroll > 0) {
                textView.scrollTo(0, bottomScroll);
            }
        }
    }

    /*
        onClick listeners
     */

    public void onClearClicked(View view) {
        firstNum = null;
        function = new Functions.FlipConst();

        clearSecondNum();
        textView.setText("");
    }

    public void onDigitClicked(View view) {
        Button button = (Button) view;

        if (secondNumStr.length() == 1 && secondNumStr.charAt(0) == '0') {
            secondNumStr.delete(0, 1);
        }

        secondNumStr.append(button.getText());

        textView.setText(secondNumStr.toString());
        scrollToBottom();
    }

    public void onDotClicked(View view) {
        if (!isDotEntered) {
            if (secondNumStr.length() == 0) {
                secondNumStr.append('0');
            }
            isDotEntered = true;
            secondNumStr.append('.');
            textView.setText(secondNumStr.toString());
            scrollToBottom();
        }
    }

    public void onSignClicked(View view) {
        if (secondNumStr.length() > 0 &&
                !(secondNumStr.length() == 1 && secondNumStr.charAt(0) == '0')) {
            if (secondNumStr.charAt(0) != '-') {
                secondNumStr.insert(0, '-');
            } else {
                secondNumStr.delete(0, 1);
            }
            textView.setText(secondNumStr.toString());
        }
    }

    /*
        Binary function listeners
     */

    public void onFunctionClicked(Functions.Function newFunction) {
        if (secondNumStr.length() == 0) {
            if (firstNum != null) {
                function = newFunction;
            }
        } else {
            try {
                firstNum = function.execute(firstNum, new BigDecimal(secondNumStr.toString()));
                function = newFunction;
                firstNum = firstNum.stripTrailingZeros();
                textView.setText(firstNum.toPlainString());
                clearSecondNum();
            } catch (ArithmeticException e) {
                onClearClicked(null);
                textView.setText(R.string.error);
            }
        }
    }

    public void onEqClicked(View view) {
        onFunctionClicked(new Functions.FlipConst());
    }

    public void onAddClicked(View view) {
        onFunctionClicked(new Functions.Add());
    }

    public void onSubClicked(View view) {
        onFunctionClicked(new Functions.Subtract());
    }

    public void onMulClicked(View view) {
        onFunctionClicked(new Functions.Multiply());
    }

    public void onDivClicked(View view) {
        onFunctionClicked(new Functions.Divide());
    }

}

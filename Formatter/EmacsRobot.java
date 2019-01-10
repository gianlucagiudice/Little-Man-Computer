import java.awt.*;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;

public class EmacsRobot {
    public static void main(String[] args) {
        try {
            Robot robot = new Robot();

            Thread.sleep(3000);

            // Simulate a mouse click
            robot.mousePress(InputEvent.BUTTON1_MASK);
            robot.mouseRelease(InputEvent.BUTTON1_MASK);

            // Tab a key press
            do {
                robot.keyPress(KeyEvent.VK_TAB);
                robot.keyRelease(KeyEvent.VK_TAB);
                robot.keyPress(KeyEvent.VK_DOWN);
                robot.keyRelease(KeyEvent.VK_DOWN);
                Thread.sleep(10);
            }while(true);

        } catch (Exception e) {
            e.printStackTrace();
        }

    }
}

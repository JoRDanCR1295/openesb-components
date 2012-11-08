/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.zaz.ssapi.protocol.common.util;

import com.zaz.ssapi.protocol.common.model.CustomizeOutputInfo;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author tianlize
 */
public class EmulatorOutputDataGenerator {

    public static void serialize(CustomizeOutputInfo outputInfo, File dir, String fileName) {
        try {
            FileOutputStream fos = new FileOutputStream(new File(dir.getPath() + File.separator+"src"+File.separator + fileName));
            ObjectOutputStream oos = new ObjectOutputStream(fos);
            oos.writeObject(outputInfo);
        } catch (FileNotFoundException ex) {
            Logger.getLogger(EmulatorOutputDataGenerator.class.getName()).log(Level.SEVERE, null, ex);
        } catch (IOException ex) {
            Logger.getLogger(EmulatorOutputDataGenerator.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    public static CustomizeOutputInfo getObject(File dir, String fileName) {
        CustomizeOutputInfo ret = null;
        try {
            FileInputStream fis = new FileInputStream(new File(dir.getPath() + File.separator + "src" + File.separator + fileName));
            ObjectInputStream ois = new ObjectInputStream(fis);
            ret = (CustomizeOutputInfo) ois.readObject();
        } catch (FileNotFoundException ex) {
            Logger.getLogger(EmulatorOutputDataGenerator.class.getName()).log(Level.SEVERE, null, ex);
        } catch (ClassNotFoundException ex) {
            Logger.getLogger(EmulatorOutputDataGenerator.class.getName()).log(Level.SEVERE, null, ex);
        } catch (IOException ex) {
            Logger.getLogger(EmulatorOutputDataGenerator.class.getName()).log(Level.SEVERE, null, ex);
        }
        return ret;
    }
}

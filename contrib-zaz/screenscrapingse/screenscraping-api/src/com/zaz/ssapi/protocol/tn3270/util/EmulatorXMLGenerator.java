/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.zaz.ssapi.protocol.tn3270.util;

import com.zaz.ssapi.protocol.tn3270.ProtocolHandler3270;
import com.zaz.ssapi.protocol.tn3270.model.jaxb.Emulatorflow;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

/**
 *
 * @author xliu2
 */
public class EmulatorXMLGenerator {

    public EmulatorXMLGenerator() {
    }

    public static void generateXMLFile(File dir, String filename, ProtocolHandler3270 handler) { 
        File file = new File(dir.getAbsolutePath() + File.separator + "src" 
                + File.separator + filename);
        writeXML(file, handler.getEmulatorflow());
        emulatorflowClear(handler.getEmulatorflow());
    }

    public static void writeXML(File file, Emulatorflow emulatorflow) {
        try {                 
            JAXBContext context = JAXBContext.newInstance(Emulatorflow.class);
            Marshaller m = context.createMarshaller();
            m.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
            Writer writer = new FileWriter(file);
            m.marshal(emulatorflow, writer);
            writer.flush();
            writer.close();
        } catch (IOException ex) {
            Logger.getLogger(EmulatorXMLGenerator.class.getName()).log(Level.SEVERE, null, ex);
        } catch (JAXBException jex) {
            Logger.getLogger(EmulatorXMLGenerator.class.getName()).log(Level.SEVERE, null, jex);
        }
    }
    public static void emulatorflowClear(Emulatorflow emulatorflow) {
        for (int i = 0; i < emulatorflow.getScreen().size(); i++) {
            Emulatorflow.Screen screen = emulatorflow.getScreen().get(i);
            screen.getField().clear();
        }
        emulatorflow.getScreen().clear();
        emulatorflow = new Emulatorflow();
    }
}

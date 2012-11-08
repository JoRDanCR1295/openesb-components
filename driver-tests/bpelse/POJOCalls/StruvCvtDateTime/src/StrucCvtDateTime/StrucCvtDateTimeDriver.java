/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package StrucCvtDateTime;

import java.util.logging.Level;
import java.util.logging.Logger;
import org.netbeans.xml.schema.cvtdatemessages.ElRequest;
import org.netbeans.xml.schema.cvtdatemessages.ElResponse;

/**
 *
 * @author mczapski
 */
public class StrucCvtDateTimeDriver {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {

        ElRequest stReq = new ElRequest();
        stReq.setElDateString("2008/10/12");
        stReq.setElFormatIn("yyyy/MM/dd");
        stReq.setElFormatOut("yyyy-MM-dd'T'hh:mm:ss.SSS");
        
        ElResponse stRes = StrucCvtDateTime.CvtDateTime(stReq);
        
        Logger lgr = Logger.getLogger("WSCvtDate");
        lgr.log(Level.INFO, "Date is " + stRes.getElDateString());
        
    }

}

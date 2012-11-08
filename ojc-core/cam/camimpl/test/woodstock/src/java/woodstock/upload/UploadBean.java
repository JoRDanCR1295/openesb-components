/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)UploadBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package woodstock.upload;

import java.io.Serializable;
import com.sun.webui.jsf.model.UploadedFile;
import javax.faces.application.FacesMessage;
import javax.faces.context.FacesContext;

/**
 *
 * @author ylee
 */
public class UploadBean implements Serializable  {
    
    private UploadedFile uploadedFile;
    private String fileName;
     
    /** Creates a new instance of UploadBean */
    public UploadBean() {
    }
    
    /**
     * Getter for property uploadedFile.
     * @return Value of property uploadedFile.
     */
    public UploadedFile getUploadedFile() {
        return this.uploadedFile;
    }
    
    /**
     * Setter for property uploadedFile.
     * @param uploadedFile New value of property uploadedFile.
     */
    public void setUploadedFile(UploadedFile uploadedFile) {
        System.out.println("setUploadedFile called: "+uploadedFile);
        this.uploadedFile = uploadedFile;
        if ( uploadedFile!=null ) {
            System.out.println(">>>> uploadFile.originalName="+uploadedFile.getOriginalName());
        }
    }    
    
    public String actionHandler() {
        System.out.println(">>>>> actionHandler called..." + uploadedFile);
        if ( uploadedFile!=null ) {
            System.out.println(">>>> uploadFile.originalName="+uploadedFile.getOriginalName());
            FacesContext.getCurrentInstance().addMessage(null, new FacesMessage(FacesMessage.SEVERITY_INFO, uploadedFile.getOriginalName(), null));
        } else {
            FacesContext.getCurrentInstance().addMessage(null, new FacesMessage(FacesMessage.SEVERITY_INFO, "NULL filename", null));
        }
        return "";
    }
    
     /**
     *Getter method for fileName.
     */
    public String getFileName() {
           return fileName;
    }
   
    /**
     *Setter method for fileName.
     */
    public void setFileName(String fileName) {
           this.fileName = fileName;
    }   
    
}

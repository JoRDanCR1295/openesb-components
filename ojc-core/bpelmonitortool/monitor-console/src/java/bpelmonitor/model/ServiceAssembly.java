/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package bpelmonitor.model;

import java.io.Serializable;

/**
 *
 * @author mbhasin
 */
public class ServiceAssembly implements Serializable {
    
    private String name;

    public ServiceAssembly(String name) {
        this.name = name;
    }
    /**
     * @return the name
     */
    public String getName() {
        return name;
    }
}

/*
 * StatsSvc.java
 *
 * Created on March 28, 2007, 1:04 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.sun.soabi.proceng1ejb;

import javax.ejb.Stateless;
import javax.jws.WebService;

/**
 *
 * @author fkieviet
 */

@Stateless()
@WebService()
public class StatsSvc {
    public static Stats sStats = new Stats();
    
    public Stats getStats() {
        return sStats.copy();
    }
}

package it.imolinfo.jbi4corba.test.testproviderarray;


/**
* it/imolinfo/jbi4corba/test/testproviderarray/_ArrayInterfaceStub.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from EchoArray.idl
* venerd� 20 febbraio 2009 12.20.53 CET
*/

public class _ArrayInterfaceStub extends org.omg.CORBA.portable.ObjectImpl implements it.imolinfo.jbi4corba.test.testproviderarray.ArrayInterface
{

  public boolean[] echoArrayBoolean (boolean[] e)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("echoArrayBoolean", true);
                it.imolinfo.jbi4corba.test.testproviderarray.ArrayBooleanHelper.write ($out, e);
                $in = _invoke ($out);
                boolean $result[] = it.imolinfo.jbi4corba.test.testproviderarray.ArrayBooleanHelper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return echoArrayBoolean (e        );
            } finally {
                _releaseReply ($in);
            }
  } // echoArrayBoolean

  public char[] echoArrayChar (char[] e)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("echoArrayChar", true);
                it.imolinfo.jbi4corba.test.testproviderarray.ArrayCharHelper.write ($out, e);
                $in = _invoke ($out);
                char $result[] = it.imolinfo.jbi4corba.test.testproviderarray.ArrayCharHelper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return echoArrayChar (e        );
            } finally {
                _releaseReply ($in);
            }
  } // echoArrayChar

  public char[] echoArrayWChar (char[] e)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("echoArrayWChar", true);
                it.imolinfo.jbi4corba.test.testproviderarray.ArrayWCharHelper.write ($out, e);
                $in = _invoke ($out);
                char $result[] = it.imolinfo.jbi4corba.test.testproviderarray.ArrayWCharHelper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return echoArrayWChar (e        );
            } finally {
                _releaseReply ($in);
            }
  } // echoArrayWChar

  public byte[] echoArrayOctet (byte[] e)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("echoArrayOctet", true);
                it.imolinfo.jbi4corba.test.testproviderarray.ArrayOctetHelper.write ($out, e);
                $in = _invoke ($out);
                byte $result[] = it.imolinfo.jbi4corba.test.testproviderarray.ArrayOctetHelper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return echoArrayOctet (e        );
            } finally {
                _releaseReply ($in);
            }
  } // echoArrayOctet

  public String[] echoArrayString (String[] e)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("echoArrayString", true);
                it.imolinfo.jbi4corba.test.testproviderarray.ArrayStringHelper.write ($out, e);
                $in = _invoke ($out);
                String $result[] = it.imolinfo.jbi4corba.test.testproviderarray.ArrayStringHelper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return echoArrayString (e        );
            } finally {
                _releaseReply ($in);
            }
  } // echoArrayString

  public String[] echoArrayWString (String[] e)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("echoArrayWString", true);
                it.imolinfo.jbi4corba.test.testproviderarray.ArrayWStringHelper.write ($out, e);
                $in = _invoke ($out);
                String $result[] = it.imolinfo.jbi4corba.test.testproviderarray.ArrayWStringHelper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return echoArrayWString (e        );
            } finally {
                _releaseReply ($in);
            }
  } // echoArrayWString

  public short[] echoArrayShort (short[] e)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("echoArrayShort", true);
                it.imolinfo.jbi4corba.test.testproviderarray.ArrayShortHelper.write ($out, e);
                $in = _invoke ($out);
                short $result[] = it.imolinfo.jbi4corba.test.testproviderarray.ArrayShortHelper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return echoArrayShort (e        );
            } finally {
                _releaseReply ($in);
            }
  } // echoArrayShort

  public short[] echoArrayUnsignedShort (short[] e)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("echoArrayUnsignedShort", true);
                it.imolinfo.jbi4corba.test.testproviderarray.ArrayUnsignedShortHelper.write ($out, e);
                $in = _invoke ($out);
                short $result[] = it.imolinfo.jbi4corba.test.testproviderarray.ArrayUnsignedShortHelper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return echoArrayUnsignedShort (e        );
            } finally {
                _releaseReply ($in);
            }
  } // echoArrayUnsignedShort

  public int[] echoArrayLong (int[] e)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("echoArrayLong", true);
                it.imolinfo.jbi4corba.test.testproviderarray.ArrayLongHelper.write ($out, e);
                $in = _invoke ($out);
                int $result[] = it.imolinfo.jbi4corba.test.testproviderarray.ArrayLongHelper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return echoArrayLong (e        );
            } finally {
                _releaseReply ($in);
            }
  } // echoArrayLong

  public int[] echoArrayUnsignedLong (int[] e)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("echoArrayUnsignedLong", true);
                it.imolinfo.jbi4corba.test.testproviderarray.ArrayUnsignedLongHelper.write ($out, e);
                $in = _invoke ($out);
                int $result[] = it.imolinfo.jbi4corba.test.testproviderarray.ArrayUnsignedLongHelper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return echoArrayUnsignedLong (e        );
            } finally {
                _releaseReply ($in);
            }
  } // echoArrayUnsignedLong

  public long[] echoArrayLongLong (long[] e)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("echoArrayLongLong", true);
                it.imolinfo.jbi4corba.test.testproviderarray.ArrayLongLongHelper.write ($out, e);
                $in = _invoke ($out);
                long $result[] = it.imolinfo.jbi4corba.test.testproviderarray.ArrayLongLongHelper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return echoArrayLongLong (e        );
            } finally {
                _releaseReply ($in);
            }
  } // echoArrayLongLong

  public long[] echoArrayUnsignedLongLong (long[] e)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("echoArrayUnsignedLongLong", true);
                it.imolinfo.jbi4corba.test.testproviderarray.ArrayUnsignedLongLongHelper.write ($out, e);
                $in = _invoke ($out);
                long $result[] = it.imolinfo.jbi4corba.test.testproviderarray.ArrayUnsignedLongLongHelper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return echoArrayUnsignedLongLong (e        );
            } finally {
                _releaseReply ($in);
            }
  } // echoArrayUnsignedLongLong

  public float[] echoArrayFloat (float[] e)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("echoArrayFloat", true);
                it.imolinfo.jbi4corba.test.testproviderarray.ArrayFloatHelper.write ($out, e);
                $in = _invoke ($out);
                float $result[] = it.imolinfo.jbi4corba.test.testproviderarray.ArrayFloatHelper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return echoArrayFloat (e        );
            } finally {
                _releaseReply ($in);
            }
  } // echoArrayFloat

  public double[] echoArrayDouble (double[] e)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("echoArrayDouble", true);
                it.imolinfo.jbi4corba.test.testproviderarray.ArrayDoubleHelper.write ($out, e);
                $in = _invoke ($out);
                double $result[] = it.imolinfo.jbi4corba.test.testproviderarray.ArrayDoubleHelper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return echoArrayDouble (e        );
            } finally {
                _releaseReply ($in);
            }
  } // echoArrayDouble

  public it.imolinfo.jbi4corba.test.testproviderarray.ComplexStruct[] echoArrayComplexStruct (it.imolinfo.jbi4corba.test.testproviderarray.ComplexStruct[] e)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("echoArrayComplexStruct", true);
                it.imolinfo.jbi4corba.test.testproviderarray.ArrayComplexStructHelper.write ($out, e);
                $in = _invoke ($out);
                it.imolinfo.jbi4corba.test.testproviderarray.ComplexStruct $result[] = it.imolinfo.jbi4corba.test.testproviderarray.ArrayComplexStructHelper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return echoArrayComplexStruct (e        );
            } finally {
                _releaseReply ($in);
            }
  } // echoArrayComplexStruct

  public String[][] echoMatrixString (String[][] e)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("echoMatrixString", true);
                it.imolinfo.jbi4corba.test.testproviderarray.MatrixStringHelper.write ($out, e);
                $in = _invoke ($out);
                String $result[][] = it.imolinfo.jbi4corba.test.testproviderarray.MatrixStringHelper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return echoMatrixString (e        );
            } finally {
                _releaseReply ($in);
            }
  } // echoMatrixString

  // Type-specific CORBA::Object operations
  private static String[] __ids = {
    "IDL:it/imolinfo/jbi4corba/test/testproviderarray/ArrayInterface:1.0"};

  public String[] _ids ()
  {
    return (String[])__ids.clone ();
  }

  private void readObject (java.io.ObjectInputStream s) throws java.io.IOException
  {
     String str = s.readUTF ();
     String[] args = null;
     java.util.Properties props = null;
     org.omg.CORBA.Object obj = org.omg.CORBA.ORB.init (args, props).string_to_object (str);
     org.omg.CORBA.portable.Delegate delegate = ((org.omg.CORBA.portable.ObjectImpl) obj)._get_delegate ();
     _set_delegate (delegate);
  }

  private void writeObject (java.io.ObjectOutputStream s) throws java.io.IOException
  {
     String[] args = null;
     java.util.Properties props = null;
     String str = org.omg.CORBA.ORB.init (args, props).object_to_string (this);
     s.writeUTF (str);
  }
} // class _ArrayInterfaceStub

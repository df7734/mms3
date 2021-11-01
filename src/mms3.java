public class mms3 {

    static int Fact(int n){
        int result = 1;
        for (int i = 1; i <=n; i ++){
            result = result*i;
        }
        return result;
    }
    static    double [] gauss(double[][] a, double[] y, int n)
    {
        double[] x;
        double max;
        int k, index;
        final double eps = 0.00001;
        x = new double[n];
        k = 0;
        while (k < n)
        {

            max = Math.abs(a[k][k]);
            index = k;
            for (int i = k + 1; i < n; i++)
            {
                if (Math.abs(a[i][k]) > max)
                {
                    max = Math.abs(a[i][k]);
                    index = i;
                }
            }
            // Перестановка
            if (max < eps)
            {

                System.out.println("Рішення неможливо знайти");
                System.out.println(index+ " матриці A" );
                return new double[0];
            }
            for (int j = 0; j < n; j++)
            {
                double temp = a[k][j];
                a[k][j] = a[index][j];
                a[index][j] = temp;
            }
            double temp = y[k];
            y[k] = y[index];
            y[index] = temp;
            // Нормалізація
            for (int i = k; i < n; i++)
            {
                temp = a[i][k];
                if (Math.abs(temp) < eps) continue;
                for (int j = 0; j < n; j++)
                    a[i][j] = a[i][j] / temp;
                y[i] = y[i] / temp;
                if (i == k)  continue;
                for (int j = 0; j < n; j++)
                    a[i][j] = a[i][j] - a[k][j];
                y[i] = y[i] - y[k];
            }
            k++;
        }
        // зворотній хід
        for (k = n - 1; k >= 0; k--)
        {
            x[k] = y[k];
            for (int i = 0; i < k; i++)
                y[i] = y[i] - a[i][k] * x[k];
        }
        return x;
    }
static  double[] searchE(double[][] p)//знаходить еі методом Гауса по матриці ймовірностей слідування
    { final int N = 4;
     System.out.println("Розрахунок коефiцiєнтiв передачi");
        double[][] a = new double[N][N];
        double[] b =new double[N];

        //заповнення матриці
        for (int i = 0; i < N; ++i)
        {
            b[i] =  (-1)*p[0][ i+1];
        }
        for (int i = 1; i < N+1; ++i)
        {
            for (int j = 0; j < N; ++j)
            {
                a[i-1][ j ] = p[j+1][i];
                if ((i-1) == j) --a[i-1][ j];
            }
        }
        double[] Ematr1 = new double[]{1,0,0,0};
    double[] res = gauss(a , b , N);
        for(int i = 0; i<res.length; i++)
        System.out.println("e"+(i+1)+"="+res[i]);
        return res;
    }
static double[] searchC(double[] l,double[] e ,   double[] m, int[] r){
  System.out.println("Шукаємо нормуючі множники:");
    int n = 4;
        double[] C = new double[n];
        int k = 0;
        for (int i = 0 ; i<n; i++){
            if(r[i+1]==1){
                C[i] =  Math.pow((Math.pow(l[0]*e[i]*m[i+1],r[i+1])*(1/(Fact(r[i+1])*(1-((l[0]*e[i]*m[i+1])/r[i+1])))))+Math.pow(l[0]*e[i]*m[i+1],k)*(1/Fact(k)),-1);

           System.out.println("C"+(i+1)+"="+C[i]);
            }
            if(r[i+1]==2){
                C[i] =  Math.pow((Math.pow(l[0]*e[i]*m[i+1],r[i+1])*(1/(Fact(r[i+1])*(1-((l[0]*e[i]*m[i+1])/r[i+1])))))+Math.pow(l[0]*e[i]*m[i+1],k)*(1/Fact(k))+Math.pow(l[0]*e[i]*m[i+1],k+1)*(1/Fact(k+1)),-1);
                System.out.println("C"+(i+1)+"="+C[i]);
            }

        }
        double[] P = new double[4];

        for(int i = 0; i<4; i++){
            for(int j = 0; j<999 ; j++){
                if(j<=r[i+1]){
                    P[i]=P[i]+Math.pow(l[0]*e[i]*m[i+1],j)*C[i]*(1/Fact(j));
                }
              else P[i]=P[i]+Math.pow(l[0]*e[i]*m[i+1],j)*C[i]*(1/(Fact(r[i+1])*Math.pow(r[i+1],j-r[i+1])));
            }
            System.out.println("Cума від 0 до нескінченності p[k]="+ P[i]+ " отже C"+(i+1)+" порахована правильно ");
        }
return C;
}
static double[] searchL(double[] C, double[] l,double[] e ,   double[] m, int[] r){
        System.out.println("Середня к-сть вимог:");
    double[] P = new double[4];
    double[] L = new double[4];
        int n = 4;
        for(int i = 0; i<n; i++){
            for(int j= r[i+1]+1; j<99; j++){
                P[i]=Math.pow(l[0]*e[i]*m[i+1],j)*C[i]*(1/(Fact(r[i+1])*Math.pow(r[i+1],j-r[i+1])));
                L[i]= L[i]+(j-r[i+1])*P[i];
            }
            System.out.println("L"+(i+1)+"="+L[i]);
        }
        return L;
}
static double[] searchR(double[] e , double l , double[] m){
        System.out.println("Середня к-сть зайнятих пристроїв");
        int n = 4;
        double[] R = new double[4];
        for(int i = 0; i<n ; i++){
            R[i] = e[i]*m[i+1]*l;
            System.out.println("R"+(i+1)+"="+R[i]);
        }
        return R;
}
static double[] searchM(double[] L , double[] R){
        System.out.println("Середня к-сть вимог");
        double[] M = new double[4];
        int n =4;
        for(int i= 0; i<n ; i++){
            M[i] = R[i]+ L[i];
            System.out.println("M"+(i+1)+"="+M[i]);
        }
        return M;
}
static double[] searchQ(double[] L,double[] e , double l){
        System.out.println("Середній час очікування");
        int n = 4;
        double[] Q = new double[4];
        for(int i = 0; i<n; i++){
            Q[i] = L[i]/(l*e[i]);
            System.out.println("Q"+(i+1)+"="+Q[i]);
        }
return Q;
}
static double[] searchTi(double[] M, double[] e , double l){
        System.out.println("Середній час перебування вимог у СМОі");
        int n = 4;
        double[] T = new double[4];
        for (int i = 0 ; i<n;i++){
            T[i] = M[i]/(e[i]*l);
            System.out.println("T"+(i+1)+"="+T[i]);
        }
        return T;
}
static double searchT(double[] e, double[] T){
        int n = 4;
        System.out.println("Середній час перебування вимог у мережі МО");
        double Time= 0;
        for(int i = 0 ; i<n; i++){
            Time= Time+e[i]*T[i];

        }
        System.out.println("T=" + Time);
        return Time;
}

    public static void main(String[] args) {
        final  int N = 5; // к-сть смо +1
        double lambda = 0.6; //інтенсивність надходження вимог до мо
        int[] r = new int[] { 0,1,1,1,2 }; //к-сть каналів
        double[] m = new double[]{ 0 , 0.6 , 0.3 , 0.4 , 0.1 };//інтенсивність
        double[][] p = new double[][]{{0.,1.,0.,0.,0.} , {0., 0., 0.15 , 0.13 , 0.3} , {0, 1 ,0,0,0}, {0,1,0,0,0} ,{0,1,0,0,0} };
        // матриця ймовірностей слідування
        double[] e = searchE(p); // коефіцієнти передачі
        double[] l = new double[N];
        l[0] = lambda;
        System.out.println("Шукаємо надходження до смо");
        for(int i = 1 ; i<N ; i++){
            l[i] = l[0]*e[i-1];
            System.out.println("l" + i + "="+l[i]);
        }
System.out.println("Перевірка роботи смо в сталому реживі");
        for(int i = 1 ; i<N ; i++){
            if(l[0]<r[i]/(e[i-1]*m[i]))
            {
                System.out.println("CМО"+ i+ " працює в сталому режимі");
            }
            else System.out.println("CМО"+ i+ "працює не в сталому режимі");
        }
        double[] C = searchC(l,e,m,r);
        double[] L=searchL(C,l,e,m,r);
        double[] R=searchR(e,l[0],m);
        double[] M = searchM(L,R);
        double[] Q = searchQ(L,e,l[0]);
        double[] T = searchTi(M,e,l[0]);
        double Time = searchT(e,T);


        };

    }


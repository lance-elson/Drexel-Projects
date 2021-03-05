using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

// Lance Elson
namespace CheckCredit
{
    public partial class CreditCheckForm : Form
    {
        public CreditCheckForm()
        {
            InitializeComponent();
        }

        private void buttonEnter_Click(object sender, EventArgs e)
        {

                double price = double.Parse(textboxPrice.Text);
                const double creditLimit = 8000;


                if (price > creditLimit)
                {
                    labelOutput.Text = ("You have exceeded the credit limit of " +
                        creditLimit.ToString());
                }

                else
                {
                    labelOutput.Text = "Approved";
                }
           
        }
    }
}

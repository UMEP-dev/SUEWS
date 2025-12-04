import puppeteer from 'puppeteer';

const browser = await puppeteer.launch();
const page = await browser.newPage();

await page.goto('http://127.0.0.1:9000/discourse/measure.html', {
  waitUntil: 'networkidle0'
});

// Wait for fonts and measurements
await page.waitForSelector('#output');
await new Promise(r => setTimeout(r, 1000));

// Get the measurement output
const output = await page.$eval('#output', el => el.textContent);
console.log(output);

await browser.close();

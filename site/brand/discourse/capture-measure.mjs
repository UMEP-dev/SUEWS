import puppeteer from 'puppeteer';

const browser = await puppeteer.launch();
const page = await browser.newPage();

await page.setViewport({ width: 800, height: 1200, deviceScaleFactor: 2 });
await page.goto('http://127.0.0.1:9000/discourse/measure.html', {
  waitUntil: 'networkidle0'
});

// Wait for fonts and measurements
await new Promise(r => setTimeout(r, 1500));

// Get the measurement output
const output = await page.$eval('#output', el => el.textContent);
console.log(output);

// Screenshot the page
await page.screenshot({
  path: 'measure-screenshot.png',
  fullPage: true
});

console.log('\nScreenshot saved: measure-screenshot.png');

await browser.close();
